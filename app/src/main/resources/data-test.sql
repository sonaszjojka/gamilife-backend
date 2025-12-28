INSERT INTO task.task (id, title, description, user_id, category_id, difficulty_id, deadline_date, deadline_time, reward_issued)
VALUES ('11111111-1111-1111-1111-111111111111', 'Finish report', 'Finish the monthly report',
        '11111111-1111-1111-1111-111111111111', 1, 1, '2025-12-10', '12:00:00', false),
       ('22222222-2222-2222-2222-222222222222', 'Morning workout', '30 minutes jogging',
        '22222222-2222-2222-2222-222222222222', 1, 1, '2025-12-09', null, false);

INSERT INTO task.habit (id, title, description, user_id, category_id, difficulty_id, cycle_length, last_completed_date,
                        current_deadline, current_streak, longest_streak)
VALUES ('11111111-1111-1111-1111-111111111111', 'Read a book', 'Read a book for 5 minutes',
        '11111111-1111-1111-1111-111111111111',
        1, 1, 3, '10-12-2025', '12-12-2025', 15, 30);

INSERT INTO "group"."group" (id, join_code, name, admin_id, currency_symbol, members_limit, timezone, type_id)
VALUES ('11111111-1111-1111-1111-111111111111', 'FAM123', 'group 1',
        'aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa', '€', 5, 'Europe/Warsaw', 1),
       ('22222222-2222-2222-2222-222222222222', 'FRN456', 'group 2',
        'bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb', '$', 10, 'Europe/Warsaw', 2),
       ('33333333-3333-3333-3333-333333333333', 'WRK789', 'group 3',
        'cccccccc-cccc-cccc-cccc-cccccccccccc', '£', 15, 'Europe/Warsaw', 3);

INSERT INTO "group".group_member (id, group_id, user_id, joined_at, left_at, group_money, total_earned_money)
VALUES ('22222222-1111-1111-1111-111111111111', '11111111-1111-1111-1111-111111111111',
        'aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa', '2025-10-10 12:00:00', NULL,
        150, 400),
       ('33333333-1111-1111-1111-111111111111', '11111111-1111-1111-1111-111111111111',
        'dddddddd-dddd-dddd-dddd-dddddddddddd', '2025-10-11 09:00:00', NULL, 90,
        120);

INSERT INTO "group".chat_message (id, content, is_important, group_id, sender_id)
VALUES ('aaaa1111-aaaa-aaaa-aaaa-aaaaaaaaaaaa', 'Hey everyone! Meeting at 6 PM today.', true,
        '11111111-1111-1111-1111-111111111111', '22222222-1111-1111-1111-111111111111'),
       ('bbbb2222-bbbb-bbbb-bbbb-bbbbbbbbbbbb', 'Who’s up for a movie night?', false,
        '22222222-2222-2222-2222-222222222222', '22222222-1111-1111-1111-111111111111');

INSERT INTO "group".group_invitation (id, group_id, user_id, expires_at, link, status_id, token_hash)
VALUES ('aaaa9999-aaaa-aaaa-aaaa-aaaaaaaaaaaa', '11111111-1111-1111-1111-111111111111',
        'eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee', '2025-10-10 10:00:00', 'https://app/join/FAM123',
        1, 'xxxx'),

       ('bbbb9999-bbbb-bbbb-bbbb-bbbbbbbbbbbb', '22222222-2222-2222-2222-222222222222',
        'ffffffff-ffff-ffff-ffff-ffffffffffff', '2025-10-10 11:00:00', 'https://app/join/FRN456',
        2, 'xxxxxxxxx'),

       ('cccc9999-cccc-cccc-cccc-cccccccccccc', '33333333-3333-3333-3333-333333333333',
        '99999999-9999-9999-9999-999999999999', '2025-10-10 12:00:00', 'https://app/join/WRK789',
        3, 'ffffffffffff');

INSERT INTO "group".group_request (id, user_id, group_id, status_id)
VALUES ('11112222-3333-4444-5555-666677771111', '11112222-3333-4444-5555-666677778888',
        '11111111-1111-1111-1111-111111111111', 1),
       ('11112222-3333-4444-5555-666677772222', '99998888-7777-6666-5555-444433332222',
        '22222222-2222-2222-2222-222222222222', 2),
       ('11112222-3333-4444-5555-666677773333', 'aaaa5555-bbbb-cccc-dddd-eeee11112222',
        '33333333-3333-3333-3333-333333333333', 3);
