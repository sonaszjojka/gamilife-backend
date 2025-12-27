-- views
DROP VIEW app.v_activity_item;

-- foreign keys
ALTER TABLE gamification.achievement
    DROP CONSTRAINT achievement_statistic_type;

ALTER TABLE budget.budget
    DROP CONSTRAINT budget_group;

ALTER TABLE budget.budget
    DROP CONSTRAINT budget_user;

ALTER TABLE "group".chat_message
    DROP CONSTRAINT chat_message_group;

ALTER TABLE "group".chat_message
    DROP CONSTRAINT chat_message_group_member;

ALTER TABLE auth.email_verification_code
    DROP CONSTRAINT email_verification_code_user;

ALTER TABLE auth.forgot_password_code
    DROP CONSTRAINT forgot_password_code_user;

ALTER TABLE "group"."group"
    DROP CONSTRAINT group_group_type;

ALTER TABLE "group".group_invitation
    DROP CONSTRAINT group_invitation_group;

ALTER TABLE "group".group_invitation
    DROP CONSTRAINT group_invitation_group_invitation_status;

ALTER TABLE "group".group_invitation
    DROP CONSTRAINT group_invitation_user;

ALTER TABLE group_shop.group_item
    DROP CONSTRAINT group_item_group_shop;

ALTER TABLE "group".group_member
    DROP CONSTRAINT group_member_group;

ALTER TABLE "group".group_member
    DROP CONSTRAINT group_member_user;

ALTER TABLE "group".group_request
    DROP CONSTRAINT group_request_group;

ALTER TABLE "group".group_request
    DROP CONSTRAINT group_request_group_request_status;

ALTER TABLE "group".group_request
    DROP CONSTRAINT group_request_user;

ALTER TABLE group_shop.group_shop
    DROP CONSTRAINT group_shop_group;

ALTER TABLE group_task.group_task
    DROP CONSTRAINT group_task_group;

ALTER TABLE group_task.group_task_member
    DROP CONSTRAINT group_task_member_group_member;

ALTER TABLE group_task.group_task_member
    DROP CONSTRAINT group_task_member_group_task;

ALTER TABLE group_task.group_task
    DROP CONSTRAINT group_task_task;

ALTER TABLE "group"."group"
    DROP CONSTRAINT group_user;

ALTER TABLE task.habit
    DROP CONSTRAINT habit_category;

ALTER TABLE task.habit
    DROP CONSTRAINT habit_difficulty;

ALTER TABLE task.habit
    DROP CONSTRAINT habit_user;

ALTER TABLE gamification.item
    DROP CONSTRAINT item_achievement;

ALTER TABLE gamification.item
    DROP CONSTRAINT item_item_slot;

ALTER TABLE gamification.item
    DROP CONSTRAINT item_level;

ALTER TABLE gamification.item
    DROP CONSTRAINT item_rarity;

ALTER TABLE communication.notification_retry
    DROP CONSTRAINT notification_retry_notification_type;

ALTER TABLE communication.notification_retry
    DROP CONSTRAINT notification_retry_user;

ALTER TABLE budget.operation
    DROP CONSTRAINT operation_budget;

ALTER TABLE budget.operation
    DROP CONSTRAINT operation_budget_category;

ALTER TABLE budget.operation
    DROP CONSTRAINT operation_cyclic_operation;

ALTER TABLE budget.operation
    DROP CONSTRAINT operation_group_member;

ALTER TABLE budget.operation
    DROP CONSTRAINT operation_saving_goal;

ALTER TABLE group_shop.owned_group_item
    DROP CONSTRAINT owned_group_item_group_item;

ALTER TABLE group_shop.owned_group_item
    DROP CONSTRAINT owned_group_item_group_member;

ALTER TABLE pomodoro.pomodoro_item
    DROP CONSTRAINT pomodoro_item_habit;

ALTER TABLE pomodoro.pomodoro_item
    DROP CONSTRAINT pomodoro_item_task;

ALTER TABLE budget.reducing_goal
    DROP CONSTRAINT reducing_goal_budget;

ALTER TABLE budget.reducing_goal
    DROP CONSTRAINT reducing_goal_budget_category;

ALTER TABLE auth.refresh_token
    DROP CONSTRAINT refresh_token_user;

ALTER TABLE gamification.reward
    DROP CONSTRAINT reward_statistic_type;

ALTER TABLE budget.saving_goal
    DROP CONSTRAINT saving_goal_budget;

ALTER TABLE task.task
    DROP CONSTRAINT task_category;

ALTER TABLE task.task
    DROP CONSTRAINT task_difficulty;

ALTER TABLE task.task_notification
    DROP CONSTRAINT task_notification_task;

ALTER TABLE task.task
    DROP CONSTRAINT task_user;

ALTER TABLE gamification.user_achievement
    DROP CONSTRAINT user_achievement_achievement;

ALTER TABLE gamification.user_achievement
    DROP CONSTRAINT user_achievement_user;

ALTER TABLE gamification.user_inventory_item
    DROP CONSTRAINT user_inventory_item_item;

ALTER TABLE gamification.user_inventory_item
    DROP CONSTRAINT user_inventory_item_user;

ALTER TABLE "user"."user"
    DROP CONSTRAINT user_level;

ALTER TABLE auth.user_oauth_provider
    DROP CONSTRAINT user_oauth_provider_user;

ALTER TABLE gamification.user_statistic
    DROP CONSTRAINT user_statistic_statistic_type;

ALTER TABLE gamification.user_statistic
    DROP CONSTRAINT user_statistic_user;

-- tables
DROP TABLE gamification.achievement;

DROP TABLE budget.budget;

DROP TABLE budget.budget_category;

DROP TABLE "group".chat_message;

DROP TABLE budget.cyclic_operation;

DROP TABLE auth.email_verification_code;

DROP TABLE auth.forgot_password_code;

DROP TABLE "group"."group";

DROP TABLE "group".group_invitation;

DROP TABLE group_shop.group_item;

DROP TABLE "group".group_member;

DROP TABLE "group".group_request;

DROP TABLE "group".group_request_status;

DROP TABLE group_shop.group_shop;

DROP TABLE group_task.group_task;

DROP TABLE group_task.group_task_member;

DROP TABLE "group".group_type;

DROP TABLE task.habit;

DROP TABLE "group".invitation_status;

DROP TABLE gamification.item;

DROP TABLE gamification.item_slot;

DROP TABLE gamification."level";

DROP TABLE communication.notification_retry;

DROP TABLE communication.notification_type;

DROP TABLE budget.operation;

DROP TABLE group_shop.owned_group_item;

DROP TABLE pomodoro.pomodoro_item;

DROP TABLE gamification.rarity;

DROP TABLE budget.reducing_goal;

DROP TABLE auth.refresh_token;

DROP TABLE gamification.reward;

DROP TABLE budget.saving_goal;

DROP TABLE gamification.statistic_type;

DROP TABLE task.task;

DROP TABLE task.task_category;

DROP TABLE task.task_difficulty;

DROP TABLE task.task_notification;

DROP TABLE "user"."user";

DROP TABLE gamification.user_achievement;

DROP TABLE gamification.user_inventory_item;

DROP TABLE auth.user_oauth_provider;

DROP TABLE gamification.user_statistic;

-- schemas
DROP SCHEMA IF EXISTS app CASCADE;
DROP SCHEMA IF EXISTS auth CASCADE;
DROP SCHEMA IF EXISTS budget CASCADE;
DROP SCHEMA IF EXISTS communication CASCADE;
DROP SCHEMA IF EXISTS gamification CASCADE;
DROP SCHEMA IF EXISTS "group" CASCADE;
DROP SCHEMA IF EXISTS group_shop CASCADE;
DROP SCHEMA IF EXISTS group_task CASCADE;
DROP SCHEMA IF EXISTS pomodoro CASCADE;
DROP SCHEMA IF EXISTS task CASCADE;
DROP SCHEMA IF EXISTS "user" CASCADE;

-- user
DROP USER IF EXISTS app_gamilife;
