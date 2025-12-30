-- user
CREATE USER app_gamilife
    WITH PASSWORD 'placeholder';

-- schemas
CREATE SCHEMA app;
CREATE SCHEMA security;
CREATE SCHEMA budget;
CREATE SCHEMA communication;
CREATE SCHEMA gamification;
CREATE SCHEMA "group";
CREATE SCHEMA group_shop;
CREATE SCHEMA group_task;
CREATE SCHEMA pomodoro;
CREATE SCHEMA task;
CREATE SCHEMA "user";

-- privileges
GRANT USAGE, CREATE ON SCHEMA app TO app_gamilife;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA app TO app_gamilife;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA app TO app_gamilife;
ALTER DEFAULT PRIVILEGES IN SCHEMA app
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO app_gamilife;

GRANT USAGE, CREATE ON SCHEMA security TO app_gamilife;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA security TO app_gamilife;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA security TO app_gamilife;
ALTER DEFAULT PRIVILEGES IN SCHEMA security
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO app_gamilife;

GRANT USAGE, CREATE ON SCHEMA budget TO app_gamilife;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA budget TO app_gamilife;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA budget TO app_gamilife;
ALTER DEFAULT PRIVILEGES IN SCHEMA budget
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO app_gamilife;

GRANT USAGE, CREATE ON SCHEMA communication TO app_gamilife;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA communication TO app_gamilife;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA communication TO app_gamilife;
ALTER DEFAULT PRIVILEGES IN SCHEMA communication
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO app_gamilife;

GRANT USAGE, CREATE ON SCHEMA gamification TO app_gamilife;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA gamification TO app_gamilife;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA gamification TO app_gamilife;
ALTER DEFAULT PRIVILEGES IN SCHEMA gamification
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO app_gamilife;

GRANT USAGE, CREATE ON SCHEMA "group" TO app_gamilife;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA "group" TO app_gamilife;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA "group" TO app_gamilife;
ALTER DEFAULT PRIVILEGES IN SCHEMA "group"
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO app_gamilife;

GRANT USAGE, CREATE ON SCHEMA group_shop TO app_gamilife;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA group_shop TO app_gamilife;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA group_shop TO app_gamilife;
ALTER DEFAULT PRIVILEGES IN SCHEMA group_shop
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO app_gamilife;

GRANT USAGE, CREATE ON SCHEMA group_task TO app_gamilife;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA group_task TO app_gamilife;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA group_task TO app_gamilife;
ALTER DEFAULT PRIVILEGES IN SCHEMA group_task
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO app_gamilife;

GRANT USAGE, CREATE ON SCHEMA pomodoro TO app_gamilife;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA pomodoro TO app_gamilife;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA pomodoro TO app_gamilife;
ALTER DEFAULT PRIVILEGES IN SCHEMA pomodoro
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO app_gamilife;

GRANT USAGE, CREATE ON SCHEMA task TO app_gamilife;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA task TO app_gamilife;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA task TO app_gamilife;
ALTER DEFAULT PRIVILEGES IN SCHEMA task
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO app_gamilife;

GRANT USAGE, CREATE ON SCHEMA "user" TO app_gamilife;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA "user" TO app_gamilife;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA "user" TO app_gamilife;
ALTER DEFAULT PRIVILEGES IN SCHEMA "user"
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO app_gamilife;;

-- tables
-- Table: achievement
CREATE TABLE gamification.achievement
(
    id                uuid         NOT NULL,
    name              varchar(100) NOT NULL,
    description       varchar(250) NOT NULL,
    image_path        varchar(255) NOT NULL,
    goal              int          NOT NULL,
    money_reward      int          NOT NULL,
    experience_reward int          NOT NULL,
    statistic_type_id int          NOT NULL,
    CONSTRAINT achievement_pk PRIMARY KEY (id)
);

CREATE INDEX idx_achievement_statistic_type_id on gamification.achievement (statistic_type_id ASC);

-- Table: budget
CREATE TABLE budget.budget
(
    id         uuid                     NOT NULL,
    balance    money                    NOT NULL,
    user_id    uuid                     NULL,
    group_id   uuid                     NULL,
    version    bigint                   NOT NULL DEFAULT 0,
    created_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT budget_pk PRIMARY KEY (id)
);

CREATE INDEX idx_budget_user_id on budget.budget (user_id ASC);

CREATE INDEX idx_budget_group_id on budget.budget (group_id ASC);

-- Table: budget_category
CREATE TABLE budget.budget_category
(
    id    int          NOT NULL,
    title varchar(100) NOT NULL,
    CONSTRAINT budget_category_pk PRIMARY KEY (id)
);

-- Table: chat_message
CREATE TABLE "group".chat_message
(
    id           uuid                     NOT NULL,
    content      varchar(255)             NOT NULL,
    group_id     uuid                     NOT NULL,
    sender_id    uuid                     NOT NULL,
    is_important boolean                  NOT NULL,
    version      bigint                   NOT NULL DEFAULT 0,
    created_at   timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at   timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT chat_message_pk PRIMARY KEY (id)
);

CREATE INDEX idx_chat_message_group_id on "group".chat_message (group_id ASC);

CREATE INDEX idx_chat_message_sender_id on "group".chat_message (sender_id ASC);

-- Table: cyclic_operation
CREATE TABLE budget.cyclic_operation
(
    id           int                      NOT NULL,
    cycle_length time                     NOT NULL,
    version      bigint                   NOT NULL DEFAULT 0,
    created_at   timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at   timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT cyclic_operation_pk PRIMARY KEY (id)
);

-- Table: email_verification_code
CREATE TABLE security.email_verification_code
(
    id         uuid                     NOT NULL,
    user_id    uuid                     NOT NULL,
    code       varchar(255)             NOT NULL,
    issued_at  timestamp with time zone NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    revoked    boolean                  NOT NULL,
    version    bigint                   NOT NULL DEFAULT 0,
    created_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT email_verification_code_pk PRIMARY KEY (id)
);

CREATE INDEX idx_email_verification_code_user_id on security.email_verification_code (user_id ASC);

-- Table: forgot_password_code
CREATE TABLE security.forgot_password_code
(
    id         uuid                     NOT NULL,
    user_id    uuid                     NOT NULL,
    code       varchar(255)             NOT NULL,
    issued_at  timestamp with time zone NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    revoked    boolean                  NOT NULL,
    version    bigint                   NOT NULL DEFAULT 0,
    created_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT forgot_password_code_pk PRIMARY KEY (id)
);

CREATE INDEX idx_forgot_password_code_user_id on security.forgot_password_code (user_id ASC);

-- Table: group
CREATE TABLE "group"."group"
(
    id              uuid                     NOT NULL,
    name            varchar(100)             NOT NULL,
    join_code       varchar(20)              NOT NULL,
    admin_id        uuid                     NOT NULL,
    currency_symbol char(1)                  NOT NULL,
    type_id         int                      NOT NULL,
    members_limit   int                      NOT NULL,
    timezone        varchar(100)             NOT NULL,
    version         bigint                   NOT NULL DEFAULT 0,
    created_at      timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at      timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT group_pk PRIMARY KEY (id)
);

CREATE INDEX idx_group_admin_id on "group"."group" (admin_id ASC);

CREATE INDEX idx_group_group_type_id on "group"."group" (type_id ASC);

-- Table: group_invitation
CREATE TABLE "group".group_invitation
(
    id         uuid                     NOT NULL,
    group_id   uuid                     NOT NULL,
    user_id    uuid                     NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    link       varchar(200)             NOT NULL,
    token_hash varchar(255)             NOT NULL,
    status_id  int                      NOT NULL,
    version    bigint                   NOT NULL DEFAULT 0,
    created_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT group_invitation_pk PRIMARY KEY (id)
);

CREATE INDEX idx_group_invitation_user_id on "group".group_invitation (user_id ASC);

CREATE INDEX idx_group_invitation_group_id on "group".group_invitation (group_id ASC);

CREATE INDEX idx_group_invitation_invitation_status_id on "group".group_invitation (status_id ASC);

-- Table: group_item
CREATE TABLE group_shop.group_item
(
    id            uuid                     NOT NULL,
    name          varchar(30)              NOT NULL,
    price         int                      NOT NULL,
    is_active     boolean                  NOT NULL,
    group_shop_id uuid                     NOT NULL,
    version       bigint                   NOT NULL DEFAULT 0,
    created_at    timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at    timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT group_item_pk PRIMARY KEY (id)
);

CREATE INDEX idx_group_item_group_shop_id on group_shop.group_item (group_shop_id ASC);

-- Table: group_member
CREATE TABLE "group".group_member
(
    id                 uuid                     NOT NULL,
    group_id           uuid                     NOT NULL,
    user_id            uuid                     NOT NULL,
    joined_at          timestamp with time zone NOT NULL,
    left_at            timestamp with time zone NULL,
    group_money        int                      NOT NULL,
    total_earned_money int                      NOT NULL,
    version            bigint                   NOT NULL DEFAULT 0,
    created_at         timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at         timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT group_member_pk PRIMARY KEY (id)
);

CREATE INDEX idx_group_member_user_id on "group".group_member (user_id ASC);

CREATE INDEX idx_group_member_group_id on "group".group_member (group_id ASC);

-- Table: group_request
CREATE TABLE "group".group_request
(
    id         uuid                     NOT NULL,
    user_id    uuid                     NOT NULL,
    group_id   uuid                     NOT NULL,
    status_id  int                      NOT NULL,
    version    bigint                   NOT NULL DEFAULT 0,
    created_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT group_request_pk PRIMARY KEY (id)
);

CREATE INDEX idx_group_request_user_id on "group".group_request (user_id ASC);

CREATE INDEX idx_group_request_group_id on "group".group_request (group_id ASC);

CREATE INDEX idx_group_request_group_request_status_id on "group".group_request (status_id ASC);

-- Table: group_request_status
CREATE TABLE "group".group_request_status
(
    id    int          NOT NULL,
    title varchar(100) NOT NULL,
    CONSTRAINT group_request_status_pk PRIMARY KEY (id)
);

-- Table: group_shop
CREATE TABLE group_shop.group_shop
(
    id          uuid                     NOT NULL,
    name        varchar(100)             NOT NULL,
    description varchar(500)             NOT NULL,
    group_id    uuid                     NOT NULL,
    is_active   boolean                  NOT NULL,
    version     bigint                   NOT NULL DEFAULT 0,
    created_at  timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at  timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT group_shop_pk PRIMARY KEY (id)
);

CREATE INDEX idx_group_shop_group_id on group_shop.group_shop (group_id ASC);

-- Table: group_task
CREATE TABLE group_task.group_task
(
    id               uuid                     NOT NULL,
    group_id         uuid                     NOT NULL,
    task_id          uuid                     NOT NULL,
    reward           int                      NULL,
    accepted_at      timestamp with time zone NULL,
    decline_message  varchar(300)             NULL,
    reward_issued_at timestamp with time zone NULL,
    version          bigint                   NOT NULL DEFAULT 0,
    created_at       timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at       timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT group_task_pk PRIMARY KEY (id)
);

CREATE INDEX idx_group_task_task_id on group_task.group_task (task_id ASC);

CREATE INDEX idx_group_task_group_id on group_task.group_task (group_id ASC);

-- Table: group_task_member
CREATE TABLE group_task.group_task_member
(
    id              uuid                     NOT NULL,
    group_member_id uuid                     NOT NULL,
    group_task_id   uuid                     NOT NULL,
    marked_done_at  timestamp with time zone NULL,
    reward_issued_at timestamp with time zone NULL,
    version         bigint                   NOT NULL DEFAULT 0,
    created_at      timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at      timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT group_task_member_pk PRIMARY KEY (id)
);

CREATE INDEX idx_group_task_member_group_task_id on group_task.group_task_member (group_task_id ASC);

CREATE INDEX idx_group_task_member_group_member_id on group_task.group_task_member (group_member_id ASC);

-- Table: group_type
CREATE TABLE "group".group_type
(
    id    int         NOT NULL,
    title varchar(50) NOT NULL,
    CONSTRAINT group_type_pk PRIMARY KEY (id)
);

-- Table: habit
CREATE TABLE task.habit
(
    id                  uuid                     NOT NULL,
    title               varchar(200)             NOT NULL,
    description         varchar(500)             NULL,
    user_id             uuid                     NOT NULL,
    category_id         int                      NOT NULL,
    difficulty_id       int                      NOT NULL,
    cycle_length        int                      NOT NULL,
    current_deadline    date                     NOT NULL,
    last_completed_date date                     NULL,
    current_streak      int                      NOT NULL,
    longest_streak      int                      NOT NULL,
    version             bigint                   NOT NULL DEFAULT 0,
    created_at          timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at          timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT habit_pk PRIMARY KEY (id)
);

CREATE INDEX idx_habit_user_id on task.habit (user_id ASC);

CREATE INDEX idx_habit_category_id on task.habit (category_id ASC);

CREATE INDEX idx_habit_difficulty_id on task.habit (difficulty_id ASC);

-- Table: invitation_status
CREATE TABLE "group".invitation_status
(
    id    int          NOT NULL,
    title varchar(100) NOT NULL,
    CONSTRAINT invitation_status_pk PRIMARY KEY (id)
);

-- Table: item
CREATE TABLE gamification.item
(
    id               uuid         NOT NULL,
    name             varchar(150) NOT NULL,
    description      varchar(255) NOT NULL,
    image_path       varchar(255) NOT NULL,
    quick_sell_value int          NOT NULL,
    rarity_id        int          NOT NULL,
    item_slot_id     int          NOT NULL,
    price            int          NULL,
    achievement_id   uuid         NULL,
    unlock_level     int          NULL,
    CONSTRAINT price_higher_than_quick_sell CHECK (price > quick_sell_value) NOT DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT item_pk PRIMARY KEY (id)
);

CREATE INDEX idx_item_achievement_id on gamification.item (achievement_id ASC);

CREATE INDEX idx_item_unlock_level on gamification.item (unlock_level ASC);

CREATE INDEX idx_item_item_slot_id on gamification.item (item_slot_id ASC);

CREATE INDEX idx_item_rarity_id on gamification.item (rarity_id ASC);

-- Table: item_slot
CREATE TABLE gamification.item_slot
(
    id   int         NOT NULL,
    name varchar(20) NOT NULL,
    CONSTRAINT item_slot_pk PRIMARY KEY (id)
);

-- Table: level
CREATE TABLE gamification."level"
(
    id                  int NOT NULL,
    required_experience int NOT NULL,
    CONSTRAINT level_pk PRIMARY KEY (id)
);

-- Table: notification_retry
CREATE TABLE communication.notification_retry
(
    id                   uuid                     NOT NULL,
    user_id              uuid                     NOT NULL,
    title                varchar(100)             NOT NULL,
    message              varchar(255)             NOT NULL,
    original_timestamp   timestamp with time zone NOT NULL,
    data jsonb NULL,
    notification_type_id int                      NOT NULL,
    version              bigint                   NOT NULL DEFAULT 0,
    created_at           timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at           timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT notification_retry_pk PRIMARY KEY (id)
);

CREATE INDEX idx_notification_retry_user_id on communication.notification_retry (user_id ASC);

CREATE INDEX idx_notification_retry_notification_type_id on communication.notification_retry (notification_type_id ASC);

-- Table: notification_type
CREATE TABLE communication.notification_type
(
    id   int          NOT NULL,
    name varchar(255) NOT NULL,
    CONSTRAINT notification_type_pk PRIMARY KEY (id)
);

-- Table: operation
CREATE TABLE budget.operation
(
    id                  uuid                     NOT NULL,
    value               money                    NOT NULL,
    execution_date      timestamp with time zone NOT NULL,
    budget_category_id  int                      NOT NULL,
    cyclic_operation_id int                      NOT NULL,
    budget_id           uuid                     NOT NULL,
    group_member_id     uuid                     NULL,
    saving_goal_id      uuid                     NULL,
    version             bigint                   NOT NULL DEFAULT 0,
    created_at          timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at          timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT operation_pk PRIMARY KEY (id)
);

CREATE INDEX idx_operation_budget_id on budget.operation (budget_id ASC);

CREATE INDEX idx_operation_cyclic_operation_id on budget.operation (cyclic_operation_id ASC);

CREATE INDEX idx_operation_budget_category_id on budget.operation (budget_category_id ASC);

CREATE INDEX idx_operation_group_member_id on budget.operation (group_member_id ASC);

CREATE INDEX idx_operation_saving_goal_id on budget.operation (saving_goal_id ASC);

-- Table: owned_group_item
CREATE TABLE group_shop.owned_group_item
(
    id              uuid                     NOT NULL,
    group_member_id uuid                     NOT NULL,
    group_item_id   uuid                     NOT NULL,
    used_at         timestamp with time zone NULL,
    version         bigint                   NOT NULL DEFAULT 0,
    created_at      timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at      timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT owned_group_item_pk PRIMARY KEY (id)
);

CREATE INDEX idx_owned_group_item_group_member_id on group_shop.owned_group_item (group_member_id ASC);

CREATE INDEX idx_owned_group_item_group_item_id on group_shop.owned_group_item (group_item_id ASC);

-- Table: pomodoro_item
CREATE TABLE pomodoro.pomodoro_item
(
    id               uuid                     NOT NULL,
    cycles_completed int                      NOT NULL,
    cycles_required  int                      NOT NULL,
    reward_issued    boolean                  NOT NULL,
    task_id          uuid                     NULL,
    habit_id         uuid                     NULL,
    version          bigint                   NOT NULL DEFAULT 0,
    created_at       timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at       timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pomodoro_item_ak_1 UNIQUE (task_id) NOT DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT pomodoro_item_ak_2 UNIQUE (habit_id) NOT DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT pomodoro_task_or_habit CHECK ((task_id IS NULL AND habit_id IS NOT NULL) OR
                                             (task_id IS NOT NULL AND habit_id IS NULL)) NOT DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT pomodoro_item_pk PRIMARY KEY (id)
);

CREATE INDEX idx_pomodoro_item_habit_id on pomodoro.pomodoro_item (habit_id ASC);

CREATE INDEX idx_pomodoro_item_task_id on pomodoro.pomodoro_item (task_id ASC);

-- Table: rarity
CREATE TABLE gamification.rarity
(
    id   int         NOT NULL,
    name varchar(20) NOT NULL,
    CONSTRAINT rarity_pk PRIMARY KEY (id)
);

-- Table: reducing_goal
CREATE TABLE budget.reducing_goal
(
    id                 uuid                     NOT NULL,
    name               int                      NOT NULL,
    start_date         date                     NOT NULL,
    end_date           date                     NOT NULL,
    budget_category_id int                      NOT NULL,
    budget_id          uuid                     NOT NULL,
    reward             bigint                   NULL,
    version            bigint                   NOT NULL DEFAULT 0,
    created_at         timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at         timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT reducing_goal_pk PRIMARY KEY (id)
);

CREATE INDEX idx_reducing_goal_budget_id on budget.reducing_goal (budget_id ASC);

CREATE INDEX idx_reducing_goal_budget_category_id on budget.reducing_goal (budget_category_id ASC);

-- Table: refresh_token
CREATE TABLE security.refresh_token
(
    id         uuid                     NOT NULL,
    user_id    uuid                     NOT NULL,
    token      varchar(255)             NOT NULL,
    issued_at  timestamp with time zone NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    revoked    boolean                  NOT NULL,
    version    bigint                   NOT NULL DEFAULT 0,
    created_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT refresh_token_pk PRIMARY KEY (id)
);

CREATE INDEX idx_refresh_token_user_id on security.refresh_token (user_id ASC);

-- Table: reward
CREATE TABLE gamification.reward
(
    id                uuid NOT NULL,
    statistic_type_id int  NOT NULL,
    experience        int  NOT NULL,
    money             int  NOT NULL,
    CONSTRAINT reward_pk PRIMARY KEY (id)
);

CREATE INDEX idx_reward_statistic_type_id on gamification.reward (statistic_type_id ASC);

-- Table: saving_goal
CREATE TABLE budget.saving_goal
(
    id         uuid                     NOT NULL,
    name       varchar(250)             NOT NULL,
    deadline   date                     NOT NULL,
    goal       money                    NOT NULL,
    reward     int                      NULL,
    budget_id  uuid                     NOT NULL,
    version    bigint                   NOT NULL DEFAULT 0,
    created_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT saving_goal_pk PRIMARY KEY (id)
);

CREATE INDEX idx_saving_goal_budget_id on budget.saving_goal (budget_id ASC);

-- Table: statistic_type
CREATE TABLE gamification.statistic_type
(
    id   int          NOT NULL,
    type varchar(100) NOT NULL,
    CONSTRAINT statistic_type_pk PRIMARY KEY (id)
);

-- Table: task
CREATE TABLE task.task
(
    id            uuid                     NOT NULL,
    title         varchar(200)             NOT NULL,
    description   varchar(500)             NULL,
    user_id       uuid                     NULL,
    category_id   int                      NOT NULL,
    difficulty_id int                      NOT NULL,
    deadline_date date                     NOT NULL,
    deadline_time time                     NULL,
    completed_at  timestamp with time zone NULL,
    reward_issued boolean                  NOT NULL,
    version       bigint                   NOT NULL DEFAULT 0,
    created_at    timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at    timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT task_pk PRIMARY KEY (id)
);

CREATE INDEX idx_task_user_id on task.task (user_id ASC);

CREATE INDEX idx_task_category_id on task.task (category_id ASC);

CREATE INDEX idx_task_difficulty_id on task.task (difficulty_id ASC);

-- Table: task_category
CREATE TABLE task.task_category
(
    id    int         NOT NULL,
    name  varchar(50) NOT NULL,
    value int         NOT NULL,
    CONSTRAINT task_category_pk PRIMARY KEY (id)
);

-- Table: task_difficulty
CREATE TABLE task.task_difficulty
(
    id    int         NOT NULL,
    name  varchar(50) NOT NULL,
    value int         NOT NULL,
    CONSTRAINT task_difficulty_pk PRIMARY KEY (id)
);

-- Table: task_notification
CREATE TABLE task.task_notification
(
    id         uuid                     NOT NULL,
    task_id    uuid                     NOT NULL,
    send_at    timestamp with time zone NOT NULL,
    version    bigint                   NOT NULL DEFAULT 0,
    created_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT task_notification_pk PRIMARY KEY (id)
);

CREATE INDEX idx_task_notification_task_id on task.task_notification (task_id ASC);

-- Table: user
CREATE TABLE "user"."user"
(
    id                    uuid                     NOT NULL,
    first_name            varchar(100)             NOT NULL,
    last_name             varchar(100)             NULL,
    email                 varchar(320)             NOT NULL,
    password              varchar(200)             NULL,
    username              varchar(100)             NOT NULL,
    date_of_birth         date                     NULL,
    "level"               int                      NOT NULL,
    experience            int                      NOT NULL,
    money                 int                      NOT NULL,
    send_budget_reports   boolean                  NOT NULL,
    is_profile_public     boolean                  NOT NULL,
    is_email_verified     boolean                  NOT NULL,
    is_tutorial_completed boolean                  NOT NULL,
    timezone              varchar(100)             NOT NULL,
    last_timezone_change  timestamp with time zone NOT NULL,
    version               bigint                   NOT NULL DEFAULT 0,
    created_at            timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at            timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT user_pk PRIMARY KEY (id)
);

CREATE INDEX idx_user_level on "user"."user" ("level" ASC);

-- Table: user_achievement
CREATE TABLE gamification.user_achievement
(
    id             uuid                     NOT NULL,
    user_id        uuid                     NOT NULL,
    achievement_id uuid                     NOT NULL,
    version        bigint                   NOT NULL DEFAULT 0,
    created_at     timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at     timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT user_achievement_ak_1 UNIQUE (user_id) NOT DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT user_achievement_pk PRIMARY KEY (id)
);

CREATE INDEX idx_user_achievement_user_id on gamification.user_achievement (user_id ASC);

CREATE INDEX idx_user_achievement_achievement_id on gamification.user_achievement (achievement_id ASC);

-- Table: user_inventory_item
CREATE TABLE gamification.user_inventory_item
(
    id          uuid                     NOT NULL,
    item_id     uuid                     NOT NULL,
    user_id     uuid                     NOT NULL,
    quantity    int                      NOT NULL,
    is_equipped boolean                  NOT NULL,
    version     bigint                   NOT NULL DEFAULT 0,
    created_at  timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at  timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT user_inventory_item_pk PRIMARY KEY (id)
);

CREATE INDEX idx_user_inventory_item_item_id on gamification.user_inventory_item (item_id ASC);

CREATE INDEX idx_user_inventory_item_user_id on gamification.user_inventory_item (user_id ASC);

-- Table: user_oauth_provider
CREATE TABLE security.user_oauth_provider
(
    id          uuid                     NOT NULL,
    user_id     uuid                     NOT NULL,
    provider    varchar(255)             NOT NULL,
    provider_id varchar(255)             NOT NULL,
    version     bigint                   NOT NULL DEFAULT 0,
    created_at  timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at  timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT user_oauth_provider_pk PRIMARY KEY (id)
);

CREATE INDEX idx_user_oauth_provider_user_id on security.user_oauth_provider (user_id ASC);

-- Table: user_statistic
CREATE TABLE gamification.user_statistic
(
    id                uuid                     NOT NULL,
    user_id           uuid                     NOT NULL,
    statistic_type_id int                      NOT NULL,
    count             int                      NOT NULL,
    version           bigint                   NOT NULL DEFAULT 0,
    created_at        timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at        timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT user_statistic_ak_1 UNIQUE (user_id, statistic_type_id) NOT DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT user_statistic_pk PRIMARY KEY (id)
);

CREATE INDEX idx_user_statistic_user_id on gamification.user_statistic (user_id ASC);

CREATE INDEX idx_user_statistic_statistic_type_id on gamification.user_statistic (statistic_type_id ASC);

-- views
-- View: v_activity_item
CREATE OR REPLACE VIEW app.v_activity_item AS
SELECT t.id                AS id,
       'TASK'              AS type,
       t.title             AS title,
       t.description       AS description,
       t.user_id           AS user_id,
       t.category_id       AS category_id,
       tc.name             AS category_name,
       t.difficulty_id     AS difficulty_id,
       td.name             AS difficulty_name,
       t.deadline_date     AS deadline_date,
       t.deadline_time     AS deadline_time,
       t.completed_at      AS completed_at,
       NULL                AS cycle_length,
       NULL                AS current_streak,
       NULL                AS longest_streak,
       NULL                AS previous_deadline_date,
       pi.id               AS pomodoro_id,
       pi.cycles_completed AS cycles_completed,
       pi.cycles_required  AS cycles_required
FROM task.task t
         JOIN task.task_category tc ON t.category_id = tc.id
         JOIN task.task_difficulty td ON t.difficulty_id = td.id
         LEFT JOIN pomodoro.pomodoro_item pi ON t.id = pi.task_id
WHERE user_id IS NOT NULL -- Fetch only private tasks
UNION ALL
SELECT h.id                                                           AS id,
       'HABIT'                                                        AS type,
       h.title                                                        AS title,
       h.description                                                  AS description,
       h.user_id                                                      AS user_id,
       h.category_id                                                  AS category_id,
       tc.name                                                        AS category_name,
       h.difficulty_id                                                AS difficulty_id,
       td.name                                                        AS difficulty_name,
       h.current_deadline                                             AS deadline_date,
       NULL                                                           AS deadline_time,
       NULL                                                           AS completed_at,
       h.cycle_length                                                 AS cycle_length,
       h.current_streak                                               AS current_streak,
       h.longest_streak                                               AS longest_streak,
       (h.current_deadline - interval '1 day' * h.cycle_length)::DATE AS previous_deadline_date,
       pi.id                                                          AS pomodoro_id,
       pi.cycles_completed                                            AS cycles_completed,
       pi.cycles_required                                             AS cycles_required
FROM task.habit h
         JOIN task.task_category tc ON h.category_id = tc.id
         JOIN task.task_difficulty td ON h.difficulty_id = td.id
         LEFT JOIN pomodoro.pomodoro_item pi ON h.id = pi.habit_id;

-- foreign keys
-- Reference: achievement_statistic_type (table: achievement)
ALTER TABLE gamification.achievement
    ADD CONSTRAINT achievement_statistic_type
        FOREIGN KEY (statistic_type_id)
            REFERENCES gamification.statistic_type (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: budget_group (table: budget)
ALTER TABLE budget.budget
    ADD CONSTRAINT budget_group
        FOREIGN KEY (group_id)
            REFERENCES "group"."group" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: budget_user (table: budget)
ALTER TABLE budget.budget
    ADD CONSTRAINT budget_user
        FOREIGN KEY (user_id)
            REFERENCES "user"."user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: chat_message_group (table: chat_message)
ALTER TABLE "group".chat_message
    ADD CONSTRAINT chat_message_group
        FOREIGN KEY (group_id)
            REFERENCES "group"."group" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: chat_message_group_member (table: chat_message)
ALTER TABLE "group".chat_message
    ADD CONSTRAINT chat_message_group_member
        FOREIGN KEY (sender_id)
            REFERENCES "group".group_member (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: email_verification_code_user (table: email_verification_code)
ALTER TABLE security.email_verification_code
    ADD CONSTRAINT email_verification_code_user
        FOREIGN KEY (user_id)
            REFERENCES "user"."user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: forgot_password_code_user (table: forgot_password_code)
ALTER TABLE security.forgot_password_code
    ADD CONSTRAINT forgot_password_code_user
        FOREIGN KEY (user_id)
            REFERENCES "user"."user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_group_type (table: group)
ALTER TABLE "group"."group"
    ADD CONSTRAINT group_group_type
        FOREIGN KEY (type_id)
            REFERENCES "group".group_type (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_invitation_group (table: group_invitation)
ALTER TABLE "group".group_invitation
    ADD CONSTRAINT group_invitation_group
        FOREIGN KEY (group_id)
            REFERENCES "group"."group" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_invitation_group_invitation_status (table: group_invitation)
ALTER TABLE "group".group_invitation
    ADD CONSTRAINT group_invitation_group_invitation_status
        FOREIGN KEY (status_id)
            REFERENCES "group".invitation_status (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_invitation_user (table: group_invitation)
ALTER TABLE "group".group_invitation
    ADD CONSTRAINT group_invitation_user
        FOREIGN KEY (user_id)
            REFERENCES "user"."user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_item_group_shop (table: group_item)
ALTER TABLE group_shop.group_item
    ADD CONSTRAINT group_item_group_shop
        FOREIGN KEY (group_shop_id)
            REFERENCES group_shop.group_shop (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_member_group (table: group_member)
ALTER TABLE "group".group_member
    ADD CONSTRAINT group_member_group
        FOREIGN KEY (group_id)
            REFERENCES "group"."group" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_member_user (table: group_member)
ALTER TABLE "group".group_member
    ADD CONSTRAINT group_member_user
        FOREIGN KEY (user_id)
            REFERENCES "user"."user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_request_group (table: group_request)
ALTER TABLE "group".group_request
    ADD CONSTRAINT group_request_group
        FOREIGN KEY (group_id)
            REFERENCES "group"."group" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_request_group_request_status (table: group_request)
ALTER TABLE "group".group_request
    ADD CONSTRAINT group_request_group_request_status
        FOREIGN KEY (status_id)
            REFERENCES "group".group_request_status (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_request_user (table: group_request)
ALTER TABLE "group".group_request
    ADD CONSTRAINT group_request_user
        FOREIGN KEY (user_id)
            REFERENCES "user"."user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_shop_group (table: group_shop)
ALTER TABLE group_shop.group_shop
    ADD CONSTRAINT group_shop_group
        FOREIGN KEY (group_id)
            REFERENCES "group"."group" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_task_group (table: group_task)
ALTER TABLE group_task.group_task
    ADD CONSTRAINT group_task_group
        FOREIGN KEY (group_id)
            REFERENCES "group"."group" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_task_member_group_member (table: group_task_member)
ALTER TABLE group_task.group_task_member
    ADD CONSTRAINT group_task_member_group_member
        FOREIGN KEY (group_member_id)
            REFERENCES "group".group_member (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_task_member_group_task (table: group_task_member)
ALTER TABLE group_task.group_task_member
    ADD CONSTRAINT group_task_member_group_task
        FOREIGN KEY (group_task_id)
            REFERENCES group_task.group_task (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_task_task (table: group_task)
ALTER TABLE group_task.group_task
    ADD CONSTRAINT group_task_task
        FOREIGN KEY (task_id)
            REFERENCES task.task (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_user (table: group)
ALTER TABLE "group"."group"
    ADD CONSTRAINT group_user
        FOREIGN KEY (admin_id)
            REFERENCES "user"."user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: habit_category (table: habit)
ALTER TABLE task.habit
    ADD CONSTRAINT habit_category
        FOREIGN KEY (category_id)
            REFERENCES task.task_category (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: habit_difficulty (table: habit)
ALTER TABLE task.habit
    ADD CONSTRAINT habit_difficulty
        FOREIGN KEY (difficulty_id)
            REFERENCES task.task_difficulty (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: habit_user (table: habit)
ALTER TABLE task.habit
    ADD CONSTRAINT habit_user
        FOREIGN KEY (user_id)
            REFERENCES "user"."user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: item_achievement (table: item)
ALTER TABLE gamification.item
    ADD CONSTRAINT item_achievement
        FOREIGN KEY (achievement_id)
            REFERENCES gamification.achievement (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: item_item_slot (table: item)
ALTER TABLE gamification.item
    ADD CONSTRAINT item_item_slot
        FOREIGN KEY (item_slot_id)
            REFERENCES gamification.item_slot (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: item_level (table: item)
ALTER TABLE gamification.item
    ADD CONSTRAINT item_level
        FOREIGN KEY (unlock_level)
            REFERENCES gamification."level" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: item_rarity (table: item)
ALTER TABLE gamification.item
    ADD CONSTRAINT item_rarity
        FOREIGN KEY (rarity_id)
            REFERENCES gamification.rarity (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: notification_retry_notification_type (table: notification_retry)
ALTER TABLE communication.notification_retry
    ADD CONSTRAINT notification_retry_notification_type
        FOREIGN KEY (notification_type_id)
            REFERENCES communication.notification_type (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: notification_retry_user (table: notification_retry)
ALTER TABLE communication.notification_retry
    ADD CONSTRAINT notification_retry_user
        FOREIGN KEY (user_id)
            REFERENCES "user"."user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: operation_budget (table: operation)
ALTER TABLE budget.operation
    ADD CONSTRAINT operation_budget
        FOREIGN KEY (budget_id)
            REFERENCES budget.budget (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: operation_budget_category (table: operation)
ALTER TABLE budget.operation
    ADD CONSTRAINT operation_budget_category
        FOREIGN KEY (budget_category_id)
            REFERENCES budget.budget_category (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: operation_cyclic_operation (table: operation)
ALTER TABLE budget.operation
    ADD CONSTRAINT operation_cyclic_operation
        FOREIGN KEY (cyclic_operation_id)
            REFERENCES budget.cyclic_operation (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: operation_group_member (table: operation)
ALTER TABLE budget.operation
    ADD CONSTRAINT operation_group_member
        FOREIGN KEY (group_member_id)
            REFERENCES "group".group_member (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: operation_saving_goal (table: operation)
ALTER TABLE budget.operation
    ADD CONSTRAINT operation_saving_goal
        FOREIGN KEY (saving_goal_id)
            REFERENCES budget.saving_goal (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: owned_group_item_group_item (table: owned_group_item)
ALTER TABLE group_shop.owned_group_item
    ADD CONSTRAINT owned_group_item_group_item
        FOREIGN KEY (group_item_id)
            REFERENCES group_shop.group_item (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: owned_group_item_group_member (table: owned_group_item)
ALTER TABLE group_shop.owned_group_item
    ADD CONSTRAINT owned_group_item_group_member
        FOREIGN KEY (group_member_id)
            REFERENCES "group".group_member (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: pomodoro_item_habit (table: pomodoro_item)
ALTER TABLE pomodoro.pomodoro_item
    ADD CONSTRAINT pomodoro_item_habit
        FOREIGN KEY (habit_id)
            REFERENCES task.habit (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: pomodoro_item_task (table: pomodoro_item)
ALTER TABLE pomodoro.pomodoro_item
    ADD CONSTRAINT pomodoro_item_task
        FOREIGN KEY (task_id)
            REFERENCES task.task (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: reducing_goal_budget (table: reducing_goal)
ALTER TABLE budget.reducing_goal
    ADD CONSTRAINT reducing_goal_budget
        FOREIGN KEY (budget_id)
            REFERENCES budget.budget (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: reducing_goal_budget_category (table: reducing_goal)
ALTER TABLE budget.reducing_goal
    ADD CONSTRAINT reducing_goal_budget_category
        FOREIGN KEY (budget_category_id)
            REFERENCES budget.budget_category (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: refresh_token_user (table: refresh_token)
ALTER TABLE security.refresh_token
    ADD CONSTRAINT refresh_token_user
        FOREIGN KEY (user_id)
            REFERENCES "user"."user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: reward_statistic_type (table: reward)
ALTER TABLE gamification.reward
    ADD CONSTRAINT reward_statistic_type
        FOREIGN KEY (statistic_type_id)
            REFERENCES gamification.statistic_type (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: saving_goal_budget (table: saving_goal)
ALTER TABLE budget.saving_goal
    ADD CONSTRAINT saving_goal_budget
        FOREIGN KEY (budget_id)
            REFERENCES budget.budget (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: task_category (table: task)
ALTER TABLE task.task
    ADD CONSTRAINT task_category
        FOREIGN KEY (category_id)
            REFERENCES task.task_category (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: task_difficulty (table: task)
ALTER TABLE task.task
    ADD CONSTRAINT task_difficulty
        FOREIGN KEY (difficulty_id)
            REFERENCES task.task_difficulty (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: task_notification_task (table: task_notification)
ALTER TABLE task.task_notification
    ADD CONSTRAINT task_notification_task
        FOREIGN KEY (task_id)
            REFERENCES task.task (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: task_user (table: task)
ALTER TABLE task.task
    ADD CONSTRAINT task_user
        FOREIGN KEY (user_id)
            REFERENCES "user"."user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: user_achievement_achievement (table: user_achievement)
ALTER TABLE gamification.user_achievement
    ADD CONSTRAINT user_achievement_achievement
        FOREIGN KEY (achievement_id)
            REFERENCES gamification.achievement (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: user_achievement_user (table: user_achievement)
ALTER TABLE gamification.user_achievement
    ADD CONSTRAINT user_achievement_user
        FOREIGN KEY (user_id)
            REFERENCES "user"."user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: user_inventory_item_item (table: user_inventory_item)
ALTER TABLE gamification.user_inventory_item
    ADD CONSTRAINT user_inventory_item_item
        FOREIGN KEY (item_id)
            REFERENCES gamification.item (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: user_inventory_item_user (table: user_inventory_item)
ALTER TABLE gamification.user_inventory_item
    ADD CONSTRAINT user_inventory_item_user
        FOREIGN KEY (user_id)
            REFERENCES "user"."user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: user_level (table: user)
ALTER TABLE "user"."user"
    ADD CONSTRAINT user_level
        FOREIGN KEY ("level")
            REFERENCES gamification."level" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: user_oauth_provider_user (table: user_oauth_provider)
ALTER TABLE security.user_oauth_provider
    ADD CONSTRAINT user_oauth_provider_user
        FOREIGN KEY (user_id)
            REFERENCES "user"."user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: user_statistic_statistic_type (table: user_statistic)
ALTER TABLE gamification.user_statistic
    ADD CONSTRAINT user_statistic_statistic_type
        FOREIGN KEY (statistic_type_id)
            REFERENCES gamification.statistic_type (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: user_statistic_user (table: user_statistic)
ALTER TABLE gamification.user_statistic
    ADD CONSTRAINT user_statistic_user
        FOREIGN KEY (user_id)
            REFERENCES "user"."user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;