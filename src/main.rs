use bevy::prelude::*;
use bevy::sprite::{MaterialMesh2dBundle, Mesh2dHandle};
use bevy::render::render_asset::RenderAssetUsages;
use bevy::audio::PlaybackSettings;
use std::fs;

const GRAVITY: f32 = -1400.0;
const JUMP_VELOCITY: f32 = 600.0;
const MOVE_SPEED: f32 = 320.0;
const TILE: f32 = 40.0;

#[derive(States, Clone, Eq, PartialEq, Debug, Hash, Default)]
enum AppState { #[default] MainMenu, Playing, Victory, Defeat }

#[derive(Resource, Default)]
struct LevelIndex(usize);

#[derive(Resource)]
struct Levels(Vec<String>);

#[derive(Event, Default)]
struct LevelAdvance;

#[derive(Resource, Default)]
struct LevelAdvanceLock(bool);

#[derive(Resource, Default)]
struct LevelTransitionCooldown(u8);

#[derive(Component)]
struct MovingPlatform {
    start: Vec2,
    end: Vec2,
    speed: f32,
    last_pos: Vec2,
    t: f32,         // 0..1 在区间位置
    forward: bool,  // 来回移动方向
}

#[derive(Component)]
struct Spike;

#[derive(Resource, Default)]
struct GameStats { time_sec: f32, score: i32 }

#[derive(Component)]
struct HudText;

#[derive(Resource, Default)]
struct BgmStarted(bool);

#[derive(Component)]
struct Player;

#[derive(Component, Deref, DerefMut, Default)]
struct Velocity(Vec2);

#[derive(Component, Default)]
struct Grounded(bool);

#[derive(Component)]
struct Platform;

#[derive(Component)]
struct Goal;

#[derive(Component)]
struct CleanupOnState;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                title: "Level Rush(zya&zzt)".into(),
                resolution: (960.0, 540.0).into(),
                resizable: false,
                ..default()
            }),
            ..default()
        }))
        .insert_resource(Levels(load_levels()))
        .add_event::<LevelAdvance>()
        .init_resource::<LevelIndex>()
        .init_resource::<GameStats>()
        .init_resource::<BgmStarted>()
        .init_resource::<LevelAdvanceLock>()
        .init_resource::<LevelTransitionCooldown>()
        .insert_state(AppState::MainMenu)
        .add_systems(Startup, setup_camera)
        .add_systems(OnEnter(AppState::MainMenu), (spawn_menu, try_start_bgm))
        .add_systems(Update, menu_input.run_if(in_state(AppState::MainMenu)))
        .add_systems(OnExit(AppState::MainMenu), cleanup_state)
        .add_systems(OnEnter(AppState::Playing), (spawn_level, spawn_hud))
        .add_systems(
            Update,
            (
                movement_input,
                apply_physics,
                platform_collision,
                spike_collision,
                goal_check,
                fall_check,
                on_level_advance,
                update_moving_platforms,
                update_hud,
                tick_transition_cooldown,
            )
                .run_if(in_state(AppState::Playing)),
        )
        .add_systems(OnExit(AppState::Playing), cleanup_state)
        .add_systems(OnEnter(AppState::Victory), spawn_victory)
        .add_systems(Update, victory_input.run_if(in_state(AppState::Victory)))
        .add_systems(OnExit(AppState::Victory), cleanup_state)
        .add_systems(OnEnter(AppState::Defeat), spawn_defeat)
        .add_systems(Update, defeat_input.run_if(in_state(AppState::Defeat)))
        .add_systems(OnExit(AppState::Defeat), cleanup_state)
        .run();
}

fn setup_camera(mut commands: Commands) { commands.spawn(Camera2dBundle::default()); }
// 固定内容居中：使用正交相机并将世界原点映射到窗口中心，现有关卡坐标已以 (0,0) 为中心对称，因此直接使用默认即可。

fn spawn_menu(mut commands: Commands) {
    commands.spawn((
        TextBundle {
            text: Text::from_section(
                "Level Rush\n\nPress N to Start\nPress Q to Quit",
                TextStyle { font_size: 32.0, color: Color::WHITE, ..default() },
            ).with_justify(JustifyText::Center),
            style: Style { position_type: PositionType::Absolute, left: Val::Percent(25.0), top: Val::Percent(30.0), ..default() },
            ..default()
        },
        CleanupOnState,
    ));
}

fn menu_input(
    keys: Res<ButtonInput<KeyCode>>,
    mut next: ResMut<NextState<AppState>>,
    mut level_index: ResMut<LevelIndex>,
    mut stats: ResMut<GameStats>,
) {
    if keys.just_pressed(KeyCode::KeyN) {
        // 从第一关重新开始，并清零计分/计时
        level_index.0 = 0;
        stats.time_sec = 0.0;
        stats.score = 0;
        next.set(AppState::Playing);
    }
    if keys.just_pressed(KeyCode::KeyQ) { std::process::exit(0); }
}

fn load_levels() -> Vec<String> {
    let path = "assets/levels.txt";
    let content = fs::read_to_string(path).unwrap_or_else(|_| "".to_string());
    // 以行读取，遇到仅包含 "---" 的行作为分隔，兼容 CRLF/空格
    let mut levels: Vec<String> = Vec::new();
    let mut buf: Vec<String> = Vec::new();
    for line in content.lines() {
        if line.trim() == "---" {
            let chunk = buf.join("\n").trim().to_string();
            if !chunk.is_empty() { levels.push(chunk); }
            buf.clear();
        } else {
            buf.push(line.trim_end().to_string());
        }
    }
    let chunk = buf.join("\n").trim().to_string();
    if !chunk.is_empty() { levels.push(chunk); }
    levels
}

fn spawn_level(
    mut commands: Commands,
    levels: Res<Levels>,
    level_index: Res<LevelIndex>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    spawn_level_at(&mut commands, &levels, level_index.0, &mut meshes, &mut materials);
}

fn spawn_level_at(
    commands: &mut Commands,
    levels: &Levels,
    level_idx: usize,
    meshes: &mut Assets<Mesh>,
    materials: &mut Assets<ColorMaterial>,
) {
    let tile = TILE;
    let origin = Vec2::new(-9.5 * tile, -2.0 * tile);
    let mut player_spawn = Vec2::ZERO;

    for (row, line) in levels.0[level_idx].lines().enumerate() {
        for (col, ch) in line.chars().enumerate() {
            let x = origin.x + col as f32 * tile;
            let y = origin.y + (7 - row as i32) as f32 * tile;
            match ch {
                'X' => {
                    commands.spawn((
                        SpriteBundle {
                            transform: Transform::from_xyz(x, y, -1.0),
                            sprite: Sprite { color: Color::srgb_u8(46, 204, 113), custom_size: Some(Vec2::splat(tile)), ..default() },
                            ..default()
                        },
                        Platform,
                        CleanupOnState,
                    ));
                }
                // 使用 > 和 < 作为端点，区间内生成来回移动的平台
                '>' => { /* 端点，仅渲染标记，稍后由区间扫描创建实体 */ }
                '<' => { /* 端点 */ }
                '^' => {
                    spawn_spike_triangle(commands, Vec2::new(x, y - tile * 0.1), tile, meshes, materials);
                }
                'G' => {
                    commands.spawn((
                        SpriteBundle {
                            transform: Transform::from_xyz(x, y + tile * 0.5, -0.5),
                            sprite: Sprite { color: Color::srgba(0.9, 0.8, 0.2, 1.0), custom_size: Some(Vec2::new(tile * 0.6, tile * 1.2)), ..default() },
                            ..default()
                        },
                        Goal,
                        CleanupOnState,
                    ));
                }
                'S' => { player_spawn = Vec2::new(x, y + tile * 0.5); }
                _ => {}
            }
        }
    }

    commands.spawn((
        SpriteBundle {
            transform: Transform::from_xyz(player_spawn.x, player_spawn.y, 0.0),
            sprite: Sprite { color: Color::srgba(1.0, 0.95, 0.2, 1.0), custom_size: Some(Vec2::new(tile * 0.9, tile * 0.9)), ..default() },
            ..default()
        },
        Player,
        Velocity::default(),
        Grounded::default(),
        CleanupOnState,
    ));

    // 二次扫描：为每一行的 < ... > 创建移动平台
    for (row, line) in levels.0[level_idx].lines().enumerate() {
        let mut col = 0usize;
        let chars: Vec<char> = line.chars().collect();
        while col < chars.len() {
            if chars[col] == '<' {
                if let Some(end_col) = (col + 1..chars.len()).find(|&c| chars[c] == '>') {
                    let sx = origin.x + col as f32 * tile;
                    let ex = origin.x + end_col as f32 * tile;
                    let y = origin.y + (7 - row as i32) as f32 * tile;
                    let mid = (sx + ex) / 2.0;
                    let hash = ((row as i32 * 97 + col as i32 * 57) & 0xff) as f32;
                    let speed = 80.0 + (hash % 60.0);
                    let phase = ((row as i32 * 37 + end_col as i32 * 11) as f32).sin().abs();
                    let mid = mid;
                    commands.spawn((
                        SpriteBundle {
                            transform: Transform::from_xyz(mid, y, -1.0),
                            sprite: Sprite { color: Color::srgb_u8(52, 152, 219), custom_size: Some(Vec2::splat(tile)), ..default() },
                            ..default()
                        },
                        Platform,
                        MovingPlatform { start: Vec2::new(sx, y), end: Vec2::new(ex, y), speed, last_pos: Vec2::new(mid, y), t: phase, forward: hash as i32 % 2 == 0 },
                        CleanupOnState,
                    ));
                    col = end_col + 1;
                    continue;
                }
            }
            col += 1;
        }
    }
}

fn spawn_spike_triangle(
    commands: &mut Commands,
    pos: Vec2,
    tile: f32,
    meshes: &mut Assets<Mesh>,
    materials: &mut Assets<ColorMaterial>,
) {
    let w = tile * 0.9;
    let h = tile * 0.8;
    let half = w / 2.0;
    let points = vec![
        Vec3::new(-half, -h / 2.0, 0.0),
        Vec3::new(half, -h / 2.0, 0.0),
        Vec3::new(0.0, h / 2.0, 0.0),
    ];
    let mut mesh = Mesh::new(bevy::render::render_resource::PrimitiveTopology::TriangleList, RenderAssetUsages::RENDER_WORLD);
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, points);
    mesh.insert_indices(bevy::render::mesh::Indices::U32(vec![0, 1, 2]));
    let handle = meshes.add(mesh);
    let mat = materials.add(ColorMaterial::from(Color::srgb_u8(192, 57, 43)));
    commands.spawn((
        MaterialMesh2dBundle {
            mesh: Mesh2dHandle(handle),
            material: mat,
            transform: Transform::from_xyz(pos.x, pos.y, -0.9),
            ..default()
        },
        Spike,
        CleanupOnState,
    ));
}

fn movement_input(keys: Res<ButtonInput<KeyCode>>, mut q: Query<&mut Velocity, With<Player>>) {
    if let Ok(mut vel) = q.get_single_mut() {
        let mut x = 0.0;
        if keys.pressed(KeyCode::KeyA) || keys.pressed(KeyCode::ArrowLeft) { x -= 1.0; }
        if keys.pressed(KeyCode::KeyD) || keys.pressed(KeyCode::ArrowRight) { x += 1.0; }
        vel.x = x * MOVE_SPEED;
    }
}

fn apply_physics(time: Res<Time>, keys: Res<ButtonInput<KeyCode>>, mut q: Query<(&mut Transform, &mut Velocity, &Grounded), With<Player>>) {
    let dt = time.delta_seconds();
    if let Ok((mut t, mut v, grounded)) = q.get_single_mut() {
        if grounded.0 && (keys.just_pressed(KeyCode::Space) || keys.just_pressed(KeyCode::ArrowUp)) { v.y = JUMP_VELOCITY; }
        v.y += GRAVITY * dt;
        t.translation += Vec3::new(v.x * dt, v.y * dt, 0.0);
    }
}

fn platform_collision(
    mut player_q: Query<(&mut Transform, &mut Velocity, &mut Grounded), With<Player>>,
    platforms: Query<(&Transform, Option<&MovingPlatform>), (With<Platform>, Without<Player>)>,
) {
    let Ok((mut pt, mut pv, mut grounded)) = player_q.get_single_mut() else { return; };
    grounded.0 = false;
    let player_size = Vec2::splat(TILE * 0.9);
    for (t, moving) in platforms.iter() {
        let platform_size = Vec2::splat(TILE);
        if aabb_overlap(pt.translation.truncate(), player_size, t.translation.truncate(), platform_size) {
            let player_bottom = pt.translation.y - player_size.y / 2.0;
            let platform_top = t.translation.y + platform_size.y / 2.0;
            if player_bottom >= platform_top - 12.0 && pv.y <= 0.0 {
                pt.translation.y = platform_top + player_size.y / 2.0;
                pv.y = 0.0;
                grounded.0 = true;
                if let Some(m) = moving {
                    let dx = t.translation.x - m.last_pos.x;
                    pt.translation.x += dx;
                }
            }
        }
    }
}

fn spike_collision(
    player_q: Query<&Transform, With<Player>>,
    spikes: Query<&Transform, With<Spike>>,
    mut next: ResMut<NextState<AppState>>,
) {
    if let Ok(p) = player_q.get_single() {
        for s in spikes.iter() {
            if aabb_overlap(p.translation.truncate(), Vec2::splat(TILE * 0.8), s.translation.truncate(), Vec2::splat(TILE * 0.8)) {
                next.set(AppState::Defeat);
                break;
            }
        }
    }
}

fn goal_check(
    player_q: Query<&Transform, With<Player>>,
    goals: Query<&Transform, With<Goal>>,
    mut next: ResMut<NextState<AppState>>,
    mut level_index: ResMut<LevelIndex>,
    levels: Res<Levels>,
    mut ev_adv: EventWriter<LevelAdvance>,
    mut lock: ResMut<LevelAdvanceLock>,
    mut cooldown: ResMut<LevelTransitionCooldown>,
) {
    if lock.0 || cooldown.0 > 0 { return; }
    if let Ok(p) = player_q.get_single() {
        for g in goals.iter() {
            if p.translation.truncate().distance(g.translation.truncate()) < TILE * 0.8 {
                lock.0 = true; // 防止同一帧多次触发
                ev_adv.send(LevelAdvance);
                cooldown.0 = 10; // 接下来的若干帧忽略触发
            }
        }
    }
}

fn on_level_advance(
    mut ev_reader: EventReader<LevelAdvance>,
    mut commands: Commands,
    to_clean: Query<Entity, With<CleanupOnState>>,
    levels: Res<Levels>,
    mut level_index: ResMut<LevelIndex>,
    mut stats: ResMut<GameStats>,
    mut next: ResMut<NextState<AppState>>,
    mut lock: ResMut<LevelAdvanceLock>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    let mut handled = false;
    for _ in ev_reader.read() {
        if handled { break; }
        handled = true;
        // 计分结算（针对刚通关的关卡）
        let bonus = (50.0 - stats.time_sec).floor().max(0.0) as i32;
        stats.score += 100 + bonus;

        // 清理旧关
        for e in to_clean.iter() { commands.entity(e).despawn_recursive(); }

        // 推进关卡索引并生成下一关或进入通关界面
        if level_index.0 + 1 >= levels.0.len() {
            next.set(AppState::Victory);
            return;
        } else {
            level_index.0 += 1;
            stats.time_sec = 0.0;
            spawn_level_at(&mut commands, &levels, level_index.0, &mut meshes, &mut materials);
            spawn_hud(commands.reborrow());
        }
    }
    // 解除锁（即使本帧无事件也释放，避免卡死）
    lock.0 = false;
}

fn tick_transition_cooldown(mut cd: ResMut<LevelTransitionCooldown>) {
    if cd.0 > 0 { cd.0 -= 1; }
}

fn fall_check(player_q: Query<&Transform, With<Player>>, mut next: ResMut<NextState<AppState>>) {
    if let Ok(p) = player_q.get_single() { if p.translation.y < -600.0 { next.set(AppState::Defeat); } }
}

fn spawn_victory(mut commands: Commands) {
    commands.spawn((
        TextBundle {
            text: Text::from_section(
                "You Win!\n\nPress N to Restart\nPress Q to Quit",
                TextStyle { font_size: 32.0, color: Color::WHITE, ..default() },
            ).with_justify(JustifyText::Center),
            style: Style { position_type: PositionType::Absolute, left: Val::Percent(25.0), top: Val::Percent(30.0), ..default() },
            ..default()
        },
        CleanupOnState,
    ));
}

fn victory_input(keys: Res<ButtonInput<KeyCode>>, mut next: ResMut<NextState<AppState>>, mut level_index: ResMut<LevelIndex>) {
    if keys.just_pressed(KeyCode::KeyN) { level_index.0 = 0; next.set(AppState::Playing); }
    if keys.just_pressed(KeyCode::KeyQ) { std::process::exit(0); }
}

fn spawn_defeat(mut commands: Commands) {
    commands.spawn((
        TextBundle {
            text: Text::from_section(
                "You Died!\n\nPress R to Retry\nPress M for Menu",
                TextStyle { font_size: 32.0, color: Color::WHITE, ..default() },
            ).with_justify(JustifyText::Center),
            style: Style { position_type: PositionType::Absolute, left: Val::Percent(25.0), top: Val::Percent(30.0), ..default() },
            ..default()
        },
        CleanupOnState,
    ));
}

fn defeat_input(keys: Res<ButtonInput<KeyCode>>, mut next: ResMut<NextState<AppState>>) {
    if keys.just_pressed(KeyCode::KeyR) { next.set(AppState::Playing); }
    if keys.just_pressed(KeyCode::KeyM) { next.set(AppState::MainMenu); }
}

fn cleanup_state(mut commands: Commands, q: Query<Entity, With<CleanupOnState>>) {
    for e in q.iter() { commands.entity(e).despawn_recursive(); }
}

fn aabb_overlap(a_pos: Vec2, a_size: Vec2, b_pos: Vec2, b_size: Vec2) -> bool {
    let a_min = a_pos - a_size / 2.0;
    let a_max = a_pos + a_size / 2.0;
    let b_min = b_pos - b_size / 2.0;
    let b_max = b_pos + b_size / 2.0;
    a_min.x <= b_max.x && a_max.x >= b_min.x && a_min.y <= b_max.y && a_max.y >= b_min.y
}

fn update_moving_platforms(time: Res<Time>, mut q: Query<(&mut Transform, &mut MovingPlatform)>) {
    let dt = time.delta_seconds();
    for (mut t, mut m) in q.iter_mut() {
        m.last_pos = t.translation.truncate();
        let dir = if m.forward { 1.0 } else { -1.0 };
        let seg = (m.end - m.start).length();
        if seg > 0.0 {
            m.t += (m.speed * dt * dir) / seg;
            if m.t > 1.0 { m.t = 1.0; m.forward = false; }
            if m.t < 0.0 { m.t = 0.0; m.forward = true; }
            let pos = m.start.lerp(m.end, m.t);
            t.translation.x = pos.x;
            t.translation.y = pos.y;
        }
    }
}

fn spawn_hud(mut commands: Commands) {
    commands.spawn((
        TextBundle { text: Text::from_sections([
                TextSection::new("Lvl 1  ", TextStyle { font_size: 20.0, color: Color::WHITE, ..default() }),
                TextSection::new("Time: 0.0  ", TextStyle { font_size: 20.0, color: Color::WHITE, ..default() }),
                TextSection::new("Score: 0", TextStyle { font_size: 20.0, color: Color::WHITE, ..default() }),
            ]),
            style: Style { position_type: PositionType::Absolute, left: Val::Px(10.0), top: Val::Px(10.0), ..default() },
            ..default()
        },
        HudText,
        CleanupOnState,
    ));
}

fn update_hud(time: Res<Time>, mut stats: ResMut<GameStats>, idx: Res<LevelIndex>, mut q: Query<&mut Text, With<HudText>>) {
    stats.time_sec += time.delta_seconds();
    if let Ok(mut text) = q.get_single_mut() {
        text.sections[0].value = format!("Lvl {}  ", idx.0 + 1);
        text.sections[1].value = format!("Time: {:.1}  ", stats.time_sec);
        text.sections[2].value = format!("Score: {}", stats.score);
    }
}

fn try_start_bgm(asset_server: Res<AssetServer>, mut commands: Commands, mut started: ResMut<BgmStarted>) {
    if started.0 { return; }
    if std::fs::metadata("assets/bgm.ogg").is_ok() {
        let handle = asset_server.load("bgm.ogg");
        commands.spawn((AudioBundle { source: handle, settings: PlaybackSettings::LOOP }, CleanupOnState));
        started.0 = true;
    }
}
