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
