CREATE EXTENSION IF NOT EXISTS "pgcrypto";

-- =========================
-- Tabela: task_category
-- =========================
INSERT INTO task_category (category_id, title, value)
VALUES (1, 'Work', 1),
       (2, 'Personal', 2),
       (3, 'Health', 3);

-- =========================
-- Tabela: task_difficulty
-- =========================
INSERT INTO task_difficulty (difficulty_id, title, value)
VALUES (1, 'Easy', 1),
       (2, 'Medium', 2),
       (3, 'Hard', 3);

-- =========================
-- Tabela: task
-- =========================
INSERT INTO habit (habit_id, cycle_length, current_streak, longest_streak, created_at, accepted_date, updated_at)

VALUES ('aaaa0000-aaaa-aaaa-aaaa-aaaaaaaaaaaa', 7, 3, 5, NOW(),  NOW(), NOW()),
       ('bbbb0000-bbbb-bbbb-bbbb-bbbbbbbbbbbb',30, 10, 12, NOW(),  NULL,  NOW());

INSERT INTO task (task_id, title, start_time, end_time, category_id, difficulty_id,task_habit_id,
                  user_id, completed_at, description,is_group_task)
VALUES ('11111111-1111-1111-1111-111111111111', 'Finish report', NOW(), NOW() + INTERVAL '2 days',
        1, 2, 'aaaa0000-aaaa-aaaa-aaaa-aaaaaaaaaaaa', NULL,null,
        'Finish the monthly report',false),

       ('22222222-2222-2222-2222-222222222222', 'Morning workout', NOW(), NOW() + INTERVAL '1 day',
        3, 1, 'bbbb0000-bbbb-bbbb-bbbb-bbbbbbbbbbbb', NULL,null,
        '30 minutes jogging',false);
-- =========================
-- Tabela: habit
-- =========================





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
INSERT INTO "group" (group_id, join_code, group_name, admin_id, group_currency_symbol, members_limit, group_type_id)
VALUES ('11111111-1111-1111-1111-111111111111', 'FAM123', 'group 1', 'aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa', '€', 5, 1),
       ('22222222-2222-2222-2222-222222222222', 'FRN456', 'group 2', 'bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb', '$', 10, 2),
       ('33333333-3333-3333-3333-333333333333', 'WRK789', 'group 3', 'cccccccc-cccc-cccc-cccc-cccccccccccc', '£', 15, 3);

-- =========================
-- Tabela: group_member
-- =========================
INSERT INTO group_member (group_member_id, group_id, user_id, joined_at, left_at, group_money, total_earned_money)
VALUES ('22222222-1111-1111-1111-111111111111', '11111111-1111-1111-1111-111111111111',
        'aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa', '2025-10-10 12:00:00', NULL,
        150, 400),
       ('33333333-1111-1111-1111-111111111111', '11111111-1111-1111-1111-111111111111',
        'dddddddd-dddd-dddd-dddd-dddddddddddd', '2025-10-11 09:00:00', NULL, 90,
        120);
-- =========================
-- Tabela: chat_message
-- =========================
INSERT INTO chat_message (message_id, content, is_important, send_at, group_id, sender_id)
VALUES ('aaaa1111-aaaa-aaaa-aaaa-aaaaaaaaaaaa', 'Hey everyone! Meeting at 6 PM today.', true, '2025-10-12 12:00:00',
        '11111111-1111-1111-1111-111111111111', '22222222-1111-1111-1111-111111111111'),
       ('bbbb2222-bbbb-bbbb-bbbb-bbbbbbbbbbbb', 'Who’s up for a movie night?', false, '2025-10-12 13:00:00',
        '22222222-2222-2222-2222-222222222222', '22222222-1111-1111-1111-111111111111');

-- =========================
-- Tabela: group_invitation
-- =========================
INSERT INTO group_invitation (group_invitation_id, group_id, user_id, expires_at,
                              mail_sent_at, link, invitation_status_id, token_hash)
VALUES ('aaaa9999-aaaa-aaaa-aaaa-aaaaaaaaaaaa', '11111111-1111-1111-1111-111111111111',
        'eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee',
        '2025-10-20 00:00:00', '2025-10-10 10:00:00', 'https://app/join/FAM123', 1,
        'xxxx'),

       ('bbbb9999-bbbb-bbbb-bbbb-bbbbbbbbbbbb', '22222222-2222-2222-2222-222222222222',
        'ffffffff-ffff-ffff-ffff-ffffffffffff',
        '2025-10-20 00:00:00', '2025-10-10 11:00:00', 'https://app/join/FRN456', 2,
        'xxxxxxxxx'),

       ('cccc9999-cccc-cccc-cccc-cccccccccccc', '33333333-3333-3333-3333-333333333333',
        '99999999-9999-9999-9999-999999999999',
        '2025-10-20 00:00:00', '2025-10-10 12:00:00', 'https://app/join/WRK789', 3,
        'ffffffffffff');


-- Tabela: group_request
-- =========================
INSERT INTO group_request (group_request_id, user_id, group_id, created_at, status_id)
VALUES ('11112222-3333-4444-5555-666677771111', '11112222-3333-4444-5555-666677778888',
        '11111111-1111-1111-1111-111111111111', '2025-10-12 10:00:00', 1),
       ('11112222-3333-4444-5555-666677772222', '99998888-7777-6666-5555-444433332222',
        '22222222-2222-2222-2222-222222222222', '2025-10-12 11:00:00', 2),
       ('11112222-3333-4444-5555-666677773333', 'aaaa5555-bbbb-cccc-dddd-eeee11112222',
        '33333333-3333-3333-3333-333333333333', '2025-10-12 12:00:00', 3);
