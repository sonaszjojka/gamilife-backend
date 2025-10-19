CREATE EXTENSION IF NOT EXISTS "pgcrypto";

INSERT INTO task_category (category_id, title, value)
VALUES (1, 'Work', 1),
       (2, 'Personal', 2),
       (3, 'Health', 3);

INSERT INTO task_difficulty (difficulty_id, title, value)
VALUES (1, 'Easy', 1),
       (2, 'Medium', 2),
       (3, 'Hard', 3);

INSERT INTO habit (habit_id, cycle_length, current_streak, longest_streak, created_at, is_accepted, accepted_date,
                   decline_message, updated_at)
VALUES (gen_random_uuid(), 7, 3, 5, NOW(), true, NOW(), NULL, NOW()),
       (gen_random_uuid(), 30, 10, 12, NOW(), false, NULL, 'Too difficult', NOW());

INSERT INTO task (task_id, title, start_time, end_time, category_id, difficulty_id, user_id, completed_at, task_habit_id,
                  previous_task_id, description)
VALUES (gen_random_uuid(), 'Finish report', NOW(), NOW() + INTERVAL '2 days', 1, 2, gen_random_uuid(), NULL, NULL, NULL,
        'Finish the monthly report'),
       (gen_random_uuid(), 'Morning workout', NOW(), NOW() + INTERVAL '1 day', 3, 1, gen_random_uuid(), NULL, NULL,
        NULL, '30 minutes jogging');


-- INSERT INTO task_notification (id, send_date, task_id)
-- VALUES (1, NOW() + INTERVAL '1 hour', (SELECT task_id FROM task LIMIT 1)),
--        (2, NOW() + INTERVAL '2 hours', (SELECT task_id FROM task OFFSET 1 LIMIT 1));
-- =========================
-- Tabela: group_type
-- =========================
INSERT INTO group_type (title)
VALUES ('Open'),
       ('Closed'),
       ('Request only');

-- =========================
-- Tabela: invitation_status
-- =========================
INSERT INTO invitation_status (title)
VALUES ('Sent'),
       ('Accepted'),
       ('Declined');

-- =========================
-- Tabela: group_request_status
-- =========================
INSERT INTO group_request_status (title)
VALUES ('Sent'),
       ('Accepted'),
       ('Declined');
-- =========================
-- Tabela: "group"
-- =========================
INSERT INTO "group" (group_id, join_code, admin_id, group_currency_symbol, members_limit, group_type_id)
VALUES ('11111111-1111-1111-1111-111111111111', 'FAM123', 'aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa', '€', 5, 1),
       ('22222222-2222-2222-2222-222222222222', 'FRN456', 'bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb', '$', 10, 2),
       ('33333333-3333-3333-3333-333333333333', 'WRK789', 'cccccccc-cccc-cccc-cccc-cccccccccccc', '£', 15, 3);

-- =========================
-- Tabela: group_member
-- =========================
INSERT INTO group_member (group_id, user_id, joined_at, left_at, group_money, total_earned_money)
VALUES ('11111111-1111-1111-1111-111111111111', 'aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa', '2025-10-10 12:00:00', NULL,
        150, 400),
       ('11111111-1111-1111-1111-111111111111', 'dddddddd-dddd-dddd-dddd-dddddddddddd', '2025-10-11 09:00:00', NULL, 90,
        120),
       ('22222222-2222-2222-2222-222222222222', 'bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb', '2025-10-10 08:30:00', NULL,
        300, 600);

-- =========================
-- Tabela: chat_message
-- =========================
-- =========================
-- Tabela: group_invitation
-- =========================

-- =========================
-- Tabela: group_request
-- =========================

