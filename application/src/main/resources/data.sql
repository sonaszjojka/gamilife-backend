CREATE EXTENSION IF NOT EXISTS "pgcrypto";

-- =========================
-- Tabela: task_category
-- =========================
INSERT INTO task_category (category_id, title, value)
VALUES
    (1, 'Work', 1),
    (2, 'Personal', 2),
    (3, 'Health', 3);

-- =========================
-- Tabela: task_difficulty
-- =========================
INSERT INTO task_difficulty (difficulty_id, title, value)
VALUES
    (1, 'Easy', 1),
    (2, 'Medium', 2),
    (3, 'Hard', 3);

-- =========================
-- Tabela: habit
-- =========================
INSERT INTO habit (
    habit_id, cycle_length, current_streak, longest_streak, created_at,
    is_accepted, accepted_date, decline_message, updated_at
)
VALUES
    ('aaaa0000-aaaa-aaaa-aaaa-aaaaaaaaaaaa', 7, 3, 5, NOW(), TRUE, NOW(), NULL, NOW()),
    ('bbbb0000-bbbb-bbbb-bbbb-bbbbbbbbbbbb', 30, 10, 12, NOW(), FALSE, NULL, 'Too difficult', NOW());

-- =========================
-- Tabela: task
-- =========================
INSERT INTO task (
    task_id, title, start_time, end_time, category_id, difficulty_id,
    user_id, completed_at, task_habit_id, previous_task_id, description
)
VALUES
    ('11111111-1111-1111-1111-111111111111', 'Finish report', NOW(), NOW() + INTERVAL '2 days',
     1, 2, 'aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa', NULL, 'aaaa0000-aaaa-aaaa-aaaa-aaaaaaaaaaaa', NULL,
     'Finish the monthly report'),

    ('22222222-2222-2222-2222-222222222222', 'Morning workout', NOW(), NOW() + INTERVAL '1 day',
     3, 1, 'bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb', NULL, 'bbbb0000-bbbb-bbbb-bbbb-bbbbbbbbbbbb', NULL,
     '30 minutes jogging');

-- INSERT INTO task_notification (id, send_date, task_id)
-- VALUES (1, NOW() + INTERVAL '1 hour', (SELECT task_id FROM task LIMIT 1)),
--        (2, NOW() + INTERVAL '2 hours', (SELECT task_id FROM task OFFSET 1 LIMIT 1));
-- =========================
-- Tabela: group_type
-- =========================
INSERT INTO group_type (title)
VALUES
    ('Family'),
    ('Friends'),
    ('Work');

-- =========================
-- Tabela: invitation_status
-- =========================
INSERT INTO invitation_status (title)
VALUES
    ('Sent'),
    ('Accepted'),
    ('Declined');

-- =========================
-- Tabela: group_request_status
-- =========================
INSERT INTO group_request_status (title)
VALUES
    ('Sent'),
    ('Accepted'),
    ('Declined');

-- =========================
-- Tabela: "group"
-- =========================
INSERT INTO "group" (group_id, join_code, admin_id, group_currency_symbol, members_limit, group_type_id)
VALUES
    ('11111111-1111-1111-1111-111111111111', 'FAM123', 'aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa', '€', 5, 1),
    ('22222222-2222-2222-2222-222222222222', 'FRN456', 'bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb', '$', 10, 2),
    ('33333333-3333-3333-3333-333333333333', 'WRK789', 'cccccccc-cccc-cccc-cccc-cccccccccccc', '£', 15, 3);

-- =========================
-- Tabela: group_member
-- =========================
INSERT INTO group_member (group_id, user_id, joined_at, left_at, group_money, total_earned_money)
VALUES
    ('11111111-1111-1111-1111-111111111111', 'aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa', '2025-10-10 12:00:00', NULL, 150, 400),
    ('11111111-1111-1111-1111-111111111111', 'dddddddd-dddd-dddd-dddd-dddddddddddd', '2025-10-11 09:00:00', NULL, 90, 120),
    ('22222222-2222-2222-2222-222222222222', 'bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb', '2025-10-10 08:30:00', NULL, 300, 600);

-- =========================
-- Tabela: chat_message
-- =========================
INSERT INTO chat_message (message_id, is_important, send_at, group_id)
VALUES
    ('aaaa1111-aaaa-aaaa-aaaa-aaaaaaaaaaaa', TRUE,  '2025-10-12 12:00:00', '11111111-1111-1111-1111-111111111111'),
    ('bbbb2222-bbbb-bbbb-bbbb-bbbbbbbbbbbb', FALSE, '2025-10-12 13:00:00', '22222222-2222-2222-2222-222222222222'),
    ('cccc3333-cccc-cccc-cccc-cccccccccccc', TRUE,  '2025-10-12 14:00:00', '33333333-3333-3333-3333-333333333333');

-- =========================
-- Tabela: group_invitation
-- =========================
INSERT INTO group_invitation (
    group_invitation_id, group_id, user_id, expires_at, is_accepted,
    is_sending_email_allowed, mail_sent_at, link, invitation_status_id
)
VALUES
    ('aaaa9999-aaaa-aaaa-aaaa-aaaaaaaaaaaa', '11111111-1111-1111-1111-111111111111', 'eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee',
     '2025-10-20 00:00:00', FALSE, TRUE, '2025-10-10 10:00:00', 'https://app/join/FAM123', 1),

    ('bbbb9999-bbbb-bbbb-bbbb-bbbbbbbbbbbb', '22222222-2222-2222-2222-222222222222', 'ffffffff-ffff-ffff-ffff-ffffffffffff',
     '2025-10-20 00:00:00', TRUE, TRUE, '2025-10-10 11:00:00', 'https://app/join/FRN456', 2),

    ('cccc9999-cccc-cccc-cccc-cccccccccccc', '33333333-3333-3333-3333-333333333333', '99999999-9999-9999-9999-999999999999',
     '2025-10-20 00:00:00', FALSE, FALSE, NULL, 'https://app/join/WRK789', 3);

-- =========================
-- Tabela: group_request
-- =========================
INSERT INTO group_request (user_id, group_id, created_at, status_id)
VALUES
    ('11112222-3333-4444-5555-666677778888', '11111111-1111-1111-1111-111111111111', '2025-10-12 10:00:00', 1),
    ('99998888-7777-6666-5555-444433332222', '22222222-2222-2222-2222-222222222222', '2025-10-12 11:00:00', 2),
    ('aaaa5555-bbbb-cccc-dddd-eeee11112222', '33333333-3333-3333-3333-333333333333', '2025-10-12 12:00:00', 3);
