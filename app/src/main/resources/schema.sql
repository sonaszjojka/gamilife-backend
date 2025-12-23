-- ===================== DROP EXISTING TABLES ====================
DROP TABLE IF EXISTS "user" CASCADE;

DROP TABLE IF EXISTS pomodoro_task CASCADE;

DROP TABLE IF EXISTS habit CASCADE;
DROP TABLE IF EXISTS task CASCADE;
DROP TABLE IF EXISTS task_notification CASCADE;
DROP TABLE IF EXISTS task_category CASCADE;
DROP TABLE IF EXISTS task_difficulty CASCADE;

DROP TABLE IF EXISTS user_oauth_provider CASCADE;
DROP TABLE IF EXISTS refresh_token CASCADE;
DROP TABLE IF EXISTS email_verification_code CASCADE;
DROP TABLE IF EXISTS forgot_password_code CASCADE;

DROP TABLE IF EXISTS group_task CASCADE;
DROP TABLE IF EXISTS group_task_member CASCADE;

DROP TABLE IF EXISTS "group" CASCADE;
DROP TABLE IF EXISTS invitation_status CASCADE;
DROP TABLE IF EXISTS group_type CASCADE;
DROP TABLE IF EXISTS group_request_status CASCADE;
DROP TABLE IF EXISTS group_request CASCADE;
DROP TABLE IF EXISTS group_member CASCADE;
DROP TABLE IF EXISTS group_invitation CASCADE;
DROP TABLE IF EXISTS chat_message CASCADE;

DROP TABLE IF EXISTS group_shop CASCADE;
DROP TABLE IF EXISTS group_item_in_shop CASCADE;
DROP TABLE IF EXISTS owned_group_item CASCADE;

DROP TABLE IF EXISTS reward CASCADE;
DROP TABLE IF EXISTS rarity CASCADE;
DROP TABLE IF EXISTS achievement CASCADE;
DROP TABLE IF EXISTS statistic_type CASCADE;
DROP TABLE IF EXISTS user_statistic CASCADE;
DROP TABLE IF EXISTS item CASCADE;
DROP TABLE IF EXISTS item_slot CASCADE;
DROP TABLE IF EXISTS "level" CASCADE;
DROP TABLE IF EXISTS user_achievement CASCADE;
DROP TABLE IF EXISTS user_inventory_item CASCADE;

DROP TABLE IF EXISTS notification_retry CASCADE;
DROP TABLE IF EXISTS notification_type CASCADE;

-- ==================== TASKS ====================

CREATE TABLE task
(
    id            UUID    NOT NULL,
    title         VARCHAR(200)                NOT NULL,
    description   VARCHAR(500),
    user_id       UUID,
    category_id   INTEGER NOT NULL,
    difficulty_id INTEGER NOT NULL,
    deadline      TIMESTAMP WITH TIME ZONE,
    completed_at  TIMESTAMP WITH TIME ZONE,
    reward_issued BOOLEAN NOT NULL         DEFAULT FALSE,
    version       BIGINT  NOT NULL         DEFAULT 0,
    created_at    TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at    TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_task PRIMARY KEY (id)
);

CREATE TABLE habit
(
    id          UUID                                               NOT NULL,
    task_id     UUID                                               NOT NULL,
    cycle_length   BIGINT  NOT NULL,
    current_streak INTEGER NOT NULL,
    longest_streak INTEGER NOT NULL,
    finished_at TIMESTAMP WITH TIME ZONE,
    version     BIGINT                                             NOT NULL DEFAULT 0,
    created_at  TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at  TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_habit PRIMARY KEY (id)
);

CREATE TABLE task_category
(
    id   INTEGER     NOT NULL,
    name VARCHAR(50) NOT NULL,
    value       INTEGER     NOT NULL,
    CONSTRAINT pk_task_category PRIMARY KEY (id)
);

CREATE TABLE task_difficulty
(
    id   INTEGER     NOT NULL,
    name VARCHAR(50) NOT NULL,
    value         INTEGER     NOT NULL,
    CONSTRAINT pk_task_difficulty PRIMARY KEY (id)
);

CREATE TABLE task_notification
(
    id        INTEGER                     NOT NULL,
    task_id    UUID                                               NOT NULL,
    send_date  TIMESTAMP WITH TIME ZONE                           NOT NULL,
    version    BIGINT                                             NOT NULL DEFAULT 0,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_task_notification PRIMARY KEY (id)
);

ALTER TABLE task_notification
    ADD CONSTRAINT FK_TASK_NOTIFICATION_ON_TASK FOREIGN KEY (task_id) REFERENCES task (id);

ALTER TABLE task
    ADD CONSTRAINT FK_TASK_ON_CATEGORY FOREIGN KEY (category_id) REFERENCES task_category (id);

ALTER TABLE task
    ADD CONSTRAINT FK_TASK_ON_DIFFICULTY FOREIGN KEY (difficulty_id) REFERENCES task_difficulty (id);

ALTER TABLE habit
    ADD CONSTRAINT FK_TASK_ON_TASK_HABIT FOREIGN KEY (task_id) REFERENCES task (id);

--- Pomodoro todo users + schemas per module

CREATE TABLE pomodoro_task
(
    pomodoro_id           UUID NOT NULL,
    created_at timestamp WITH TIME ZONE,
    work_cycles_needed    INTEGER,
    work_cycles_completed INTEGER,
    task_id               UUID,
    CONSTRAINT pk_pomodoro_task PRIMARY KEY (pomodoro_id)
);
-- ==================== USER ====================

CREATE TABLE user_oauth_provider
(
    id          uuid         NOT NULL,
    user_id     uuid         NOT NULL,
    provider    varchar(255) NOT NULL,
    provider_id varchar(255) NOT NULL,
    version    bigint                                             NOT NULL DEFAULT 0,
    created_at timestamp WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_user_oauth_provider PRIMARY KEY (id)
);

CREATE TABLE refresh_token
(
    id         uuid         NOT NULL,
    user_id    uuid         NOT NULL,
    token      varchar(255) NOT NULL,
    issued_at  timestamp WITH TIME ZONE                           NOT NULL,
    expires_at timestamp WITH TIME ZONE                           NOT NULL,
    revoked    boolean      NOT NULL,
    version    bigint                                             NOT NULL DEFAULT 0,
    created_at timestamp WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_refresh_token PRIMARY KEY (id)
);

CREATE TABLE email_verification_code
(
    id         uuid         NOT NULL,
    user_id    uuid         NOT NULL,
    code       varchar(255) NOT NULL,
    issued_at  timestamp WITH TIME ZONE                           NOT NULL,
    expires_at timestamp WITH TIME ZONE                           NOT NULL,
    revoked    boolean      NOT NULL,
    version    bigint                                             NOT NULL DEFAULT 0,
    created_at timestamp WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT email_verification_code_pk PRIMARY KEY (id)
);

CREATE TABLE forgot_password_code
(
    id         UUID                                               NOT NULL,
    user_id    UUID                                               NOT NULL,
    code       VARCHAR(255)                                       NOT NULL,
    issued_at  TIMESTAMP WITH TIME ZONE                           NOT NULL,
    expires_at TIMESTAMP WITH TIME ZONE                           NOT NULL,
    revoked    BOOLEAN                                            NOT NULL,
    version    bigint                                             NOT NULL DEFAULT 0,
    created_at timestamp WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT pk_forgot_password_code PRIMARY KEY (id)
);

CREATE TABLE "user"
(
    id                    uuid         NOT NULL,
    first_name            varchar(100) NOT NULL,
    last_name             varchar(100) NOT NULL,
    email                 varchar(320) NOT NULL,
    password              varchar(200) NULL,
    username              varchar(100) NOT NULL,
    date_of_birth         date         NULL,
    experience            int          NOT NULL,
    "level"               int          NOT NULL,
    money                 int          NOT NULL,
    send_budget_reports   boolean      NOT NULL,
    is_profile_public     boolean      NOT NULL,
    is_email_verified     boolean      NOT NULL,
    is_tutorial_completed boolean      NOT NULL,
    CONSTRAINT pk_user PRIMARY KEY (id)
);

CREATE TABLE forgot_password_code
(
    id         UUID                        NOT NULL,
    user_id    UUID                        NOT NULL,
    code       VARCHAR(255)                NOT NULL,
    issued_at  timestamp WITH TIME ZONE NOT NULL,
    expires_at timestamp WITH TIME ZONE NOT NULL,
    revoked    BOOLEAN                     NOT NULL,
    CONSTRAINT pk_forgot_password_code PRIMARY KEY (id)
);

ALTER TABLE user_oauth_provider
    ADD CONSTRAINT user_oauth_provider_user
        FOREIGN KEY (user_id)
            REFERENCES "user" (id);

ALTER TABLE refresh_token
    ADD CONSTRAINT refresh_token_user
        FOREIGN KEY (user_id)
            REFERENCES "user" (id);

ALTER TABLE email_verification_code
    ADD CONSTRAINT email_validation_tokens_users
        FOREIGN KEY (user_id)
            REFERENCES "user" (id);

ALTER TABLE forgot_password_code
    ADD CONSTRAINT forgot_password_code_users
        FOREIGN KEY (user_id)
            REFERENCES "user" (id);

-- ==================== INTER-MODULE CONSTRAINTS ====================
-- TODO

-- ==================== GROUPS ====================

CREATE TABLE chat_message
(
    message_id   UUID         NOT NULL,
    content      VARCHAR(255) NOT NULL,
    is_important BOOLEAN      NOT NULL,
    sent_at timestamp WITH TIME ZONE,
    group_id     UUID         NOT NULL,
    sender_id    UUID         NOT NULL,
    CONSTRAINT pk_chat_message PRIMARY KEY (message_id)
);

CREATE TABLE "group"
(
    group_id              UUID        NOT NULL,
    join_code             VARCHAR(20) NOT NULL,
    group_name            VARCHAR(50) NOT NULL,
    admin_id              UUID        NOT NULL,
    group_currency_symbol CHAR        NOT NULL,
    members_limit         INTEGER     NOT NULL,
    group_type_id         INTEGER     NOT NULL,
    CONSTRAINT pk_group PRIMARY KEY (group_id)
);

CREATE TABLE group_invitation
(
    group_invitation_id  UUID                        NOT NULL,
    group_id             UUID                        NOT NULL,
    user_id              UUID                        NOT NULL,
    expires_at   timestamp WITH TIME ZONE NOT NULL,
    mail_sent_at timestamp WITH TIME ZONE,
    link                 VARCHAR(200)                NOT NULL,
    invitation_status_id INTEGER                     NOT NULL,
    token_hash           VARCHAR(200)                NOT NULL,
    CONSTRAINT pk_group_invitation PRIMARY KEY (group_invitation_id)
);

CREATE TABLE group_member
(
    group_member_id    UUID                        NOT NULL,
    group_id           UUID                        NOT NULL,
    user_id            UUID                        NOT NULL,
    joined_at timestamp WITH TIME ZONE NOT NULL,
    left_at   timestamp WITH TIME ZONE,
    group_money        INTEGER                     NOT NULL,
    total_earned_money INTEGER                     NOT NULL,
    CONSTRAINT pk_group_member PRIMARY KEY (group_member_id)
);

CREATE TABLE group_request
(
    group_request_id UUID    NOT NULL,
    user_id          UUID    NOT NULL,
    group_id         UUID    NOT NULL,
    created_at timestamp WITH TIME ZONE,
    status_id        INTEGER NOT NULL,
    CONSTRAINT pk_group_request PRIMARY KEY (group_request_id)
);

CREATE TABLE group_request_status
(
    group_request_status_id INTEGER      NOT NULL,
    title                   VARCHAR(100) NOT NULL,
    CONSTRAINT pk_group_request_status PRIMARY KEY (group_request_status_id)
);

CREATE TABLE group_type
(
    group_type_id INTEGER     NOT NULL,
    title         VARCHAR(50) NOT NULL,
    CONSTRAINT pk_group_type PRIMARY KEY (group_type_id)
);

CREATE TABLE invitation_status
(
    invitation_status_id INTEGER      NOT NULL,
    title                VARCHAR(100) NOT NULL,
    CONSTRAINT pk_invitation_status PRIMARY KEY (invitation_status_id)
);

ALTER TABLE "group"
    ADD CONSTRAINT uc_group_join_code UNIQUE (join_code);

ALTER TABLE chat_message
    ADD CONSTRAINT FK_CHAT_MESSAGE_ON_GROUP FOREIGN KEY (group_id) REFERENCES "group" (group_id);

ALTER TABLE group_invitation
    ADD CONSTRAINT FK_GROUP_INVITATION_ON_GROUP FOREIGN KEY (group_id) REFERENCES "group" (group_id);

ALTER TABLE group_invitation
    ADD CONSTRAINT FK_GROUP_INVITATION_ON_INVITATION_STATUS FOREIGN KEY (invitation_status_id) REFERENCES invitation_status (invitation_status_id);

ALTER TABLE group_member
    ADD CONSTRAINT FK_GROUP_MEMBER_ON_GROUP FOREIGN KEY (group_id) REFERENCES "group" (group_id);

ALTER TABLE "group"
    ADD CONSTRAINT FK_GROUP_ON_GROUP_TYPE FOREIGN KEY (group_type_id) REFERENCES group_type (group_type_id);

ALTER TABLE group_request
    ADD CONSTRAINT FK_GROUP_REQUEST_ON_GROUP FOREIGN KEY (group_id) REFERENCES "group" (group_id);

ALTER TABLE group_request
    ADD CONSTRAINT FK_GROUP_REQUEST_ON_STATUS FOREIGN KEY (status_id) REFERENCES group_request_status (group_request_status_id);
---------------------GROUP SHOP MODULE---------------------
-- Table: group_item_in_shop
CREATE TABLE group_item_in_shop
(
    group_item_in_shop_id UUID        NOT NULL,
    price                 int         NOT NULL,
    name                  varchar(30) NOT NULL,
    created_at            timestamp   NOT NULL,
    is_active             boolean     NOT NULL,
    group_shop_id         uuid        NOT NULL,
    CONSTRAINT group_item_in_shop_pk PRIMARY KEY (group_item_in_shop_id)
);

-- Table: group_shop
CREATE TABLE group_shop
(
    group_shop_id UUID         NOT NULL,
    name          varchar(100) NOT NULL,
    description   varchar(500) NOT NULL,
    group_id      uuid         NOT NULL,
    is_active     boolean      NOT NULL,
    CONSTRAINT group_shop_pk PRIMARY KEY (group_shop_id)
);

-- Table: owned_group_item
CREATE TABLE owned_group_item
(
    owned_group_item_id   UUID      NOT NULL,
    group_member_id       uuid      NOT NULL,
    group_item_in_shop_id uuid      NOT NULL,
    is_used_up            boolean   NOT NULL,
    use_date              timestamp NULL,
    CONSTRAINT owned_group_item_pk PRIMARY KEY (owned_group_item_id)
);

--foreign keys
ALTER TABLE group_item_in_shop
    ADD CONSTRAINT group_item_in_shop_group_shop
        FOREIGN KEY (group_shop_id)
            REFERENCES group_shop (group_shop_id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: group_shop_group (table: group_shop)
ALTER TABLE group_shop
    ADD CONSTRAINT group_shop_group
        FOREIGN KEY (group_id)
            REFERENCES "group" (group_id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: owned_group_item_group_item_in_shop (table: owned_group_item)
ALTER TABLE owned_group_item
    ADD CONSTRAINT owned_group_item_group_item_in_shop
        FOREIGN KEY (group_item_in_shop_id)
            REFERENCES group_item_in_shop (group_item_in_shop_id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: owned_group_item_group_member (table: owned_group_item)
ALTER TABLE owned_group_item
    ADD CONSTRAINT owned_group_item_group_member
        FOREIGN KEY (group_member_id)
            REFERENCES group_member (group_member_id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;


-- ==========================================Group Tasks==========================================
CREATE TABLE group_task
(
    group_task_id   uuid         NOT NULL,
    task_id         uuid         NOT NULL,
    group_id        uuid         NOT NULL,
    reward          int          NULL,
    is_accepted     boolean      NULL,
    accepted_date   timestamp    NULL,
    decline_message varchar(300) NULL,
    last_edit       timestamp    NULL,
    CONSTRAINT group_task_pk PRIMARY KEY (group_task_id)
);

-- Table: group_task_member
CREATE TABLE group_task_member
(
    group_task_member_id uuid    NOT NULL,
    group_task_id        uuid    NOT NULL,
    group_member_id      uuid    NOT NULL,
    is_marked_done       boolean NOT NULL,
    CONSTRAINT group_task_member_pk PRIMARY KEY (group_task_member_id)
);


ALTER TABLE group_task_member
    ADD CONSTRAINT group_task_member_group_member
        FOREIGN KEY (group_member_id)
            REFERENCES group_member (group_member_id)
;


ALTER TABLE group_task_member
    ADD CONSTRAINT group_task_member_group_task
        FOREIGN KEY (group_task_id)
            REFERENCES group_task (group_task_id)
;

ALTER TABLE group_task
    ADD CONSTRAINT group_task_group
        FOREIGN KEY (group_id)
            REFERENCES "group" (group_id)
;

ALTER TABLE group_task
    ADD CONSTRAINT group_task_task
        FOREIGN KEY (task_id)
            REFERENCES task (id)
;

-- ==========================================Gamification==========================================
-- tables
-- Table: rarity
CREATE TABLE rarity
(
    id   int         NOT NULL,
    name varchar(20) NOT NULL,
    CONSTRAINT rarity_pk PRIMARY KEY (id)
);

-- Table: achievement
CREATE TABLE achievement
(
    id                uuid         NOT NULL,
    name              varchar(100) NOT NULL,
    description       varchar(250) NOT NULL,
    image_path        varchar(255) NOT NULL,
    statistic_type_id int          NOT NULL,
    goal              int          NOT NULL,
    money_reward      int          NOT NULL,
    experience_reward int          NOT NULL,
    CONSTRAINT achievement_pk PRIMARY KEY (id)
);

-- Table: statistic_type
CREATE TABLE statistic_type
(
    id   int          NOT NULL,
    type varchar(100) NOT NULL,
    CONSTRAINT statistic_type_pk PRIMARY KEY (id)
);

-- Table: user_statistic
CREATE TABLE user_statistic
(
    id                uuid                                               NOT NULL,
    user_id           uuid                                               NOT NULL,
    statistic_type_id int                                                NOT NULL,
    count             int                                                NOT NULL,
    version           bigint                                             NOT NULL DEFAULT 0,
    created_at        timestamp WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at        timestamp WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT user_statistic_ak_1 UNIQUE (user_id, statistic_type_id) NOT DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT user_statistic_pk PRIMARY KEY (id)
);

-- Table: item
CREATE TABLE item
(
    id               uuid         NOT NULL,
    name             varchar(150) NOT NULL,
    description      varchar(255) NOT NULL,
    image_path       varchar(255) NOT NULL,
    quick_sell_value int          NOT NULL,
    item_slot_id     int          NOT NULL,
    rarity_id        int          NOT NULL,
    price            int          NULL,
    achievement_id   uuid         NULL,
    unlock_level     int          NULL,
    CONSTRAINT price_higher_than_quick_sell CHECK (price IS NULL OR price > quick_sell_value) NOT DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT acquired_in_exactly_one_way CHECK (NOT (price IS NULL AND achievement_id IS NULL AND unlock_level IS NULL)) NOT DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT item_pk PRIMARY KEY (id)
);

-- Table: item_slot
CREATE TABLE item_slot
(
    id   int         NOT NULL,
    name VARCHAR(20) NOT NULL,
    CONSTRAINT item_slot_pk PRIMARY KEY (id)
);

-- Table: level
CREATE TABLE "level"
(
    id             int NOT NULL,
    required_experience int NOT NULL,
    CONSTRAINT level_pk PRIMARY KEY (id)
);

-- Table: user_achievement
CREATE TABLE user_achievement
(
    id             uuid                                               NOT NULL,
    user_id        uuid                                               NOT NULL,
    achievement_id uuid                                               NOT NULL,
    version        bigint                                             NOT NULL DEFAULT 0,
    created_at     timestamp WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at     timestamp WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT user_achievement_ak_1 UNIQUE (user_id, achievement_id) NOT DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT user_achievement_pk PRIMARY KEY (id)
);

-- Table: user_inventory_item
CREATE TABLE user_inventory_item
(
    id          uuid                                               NOT NULL,
    item_id     uuid                                               NOT NULL,
    user_id     uuid                                               NOT NULL,
    quantity    int                                                NOT NULL,
    is_equipped boolean                                            NOT NULL,
    version     bigint                                             NOT NULL DEFAULT 0,
    created_at  timestamp WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at  timestamp WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT item_id_user_id_unique UNIQUE (item_id, user_id),
    CONSTRAINT quantity_must_be_positive CHECK (quantity > 0) NOT DEFERRABLE INITIALLY IMMEDIATE,
    CONSTRAINT user_inventory_item_pk PRIMARY KEY (id)
);

-- Table: reward
CREATE TABLE reward
(
    id                uuid NOT NULL,
    statistic_type_id int  NOT NULL,
    experience        int  NOT NULL,
    money             int  NOT NULL,
    CONSTRAINT reward_pk PRIMARY KEY (id)
);

-- Reference: achievement_statistic_type (table: achievement)
ALTER TABLE achievement
    ADD CONSTRAINT achievement_statistic_type
        FOREIGN KEY (statistic_type_id)
            REFERENCES statistic_type (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: item_rarity (table: item)
ALTER TABLE item
    ADD CONSTRAINT item_rarity
        FOREIGN KEY (rarity_id)
            REFERENCES rarity (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: item_achievement (table: item)
ALTER TABLE item
    ADD CONSTRAINT item_achievement
        FOREIGN KEY (achievement_id)
            REFERENCES achievement (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: item_item_slot (table: item)
ALTER TABLE item
    ADD CONSTRAINT item_item_slot
        FOREIGN KEY (item_slot_id)
            REFERENCES item_slot (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: item_level (table: item)
ALTER TABLE item
    ADD CONSTRAINT item_level
        FOREIGN KEY (unlock_level)
            REFERENCES "level" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: user_inventory_item_item (table: user_inventory_item)
ALTER TABLE user_inventory_item
    ADD CONSTRAINT user_inventory_item_item
        FOREIGN KEY (item_id)
            REFERENCES item (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: user_inventory_item_user (table: user_inventory_item)
ALTER TABLE user_inventory_item
    ADD CONSTRAINT user_inventory_item_user
        FOREIGN KEY (user_id)
            REFERENCES "user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: user_achievement_user (table: user_achievement)
ALTER TABLE user_achievement
    ADD CONSTRAINT user_achievement_user
        FOREIGN KEY (user_id)
            REFERENCES "user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: user_achievement_achievement (table: user_achievement)
ALTER TABLE user_achievement
    ADD CONSTRAINT user_achievement_achievement
        FOREIGN KEY (achievement_id)
            REFERENCES achievement (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: user_level (table: user)
ALTER TABLE "user"
    ADD CONSTRAINT user_level
        FOREIGN KEY ("level")
            REFERENCES "level" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: user_statistic_statistic_type (table: user_statistic)
ALTER TABLE user_statistic
    ADD CONSTRAINT user_statistic_statistic_type
        FOREIGN KEY (statistic_type_id)
            REFERENCES statistic_type (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: user_statistic_user (table: user_statistic)
ALTER TABLE user_statistic
    ADD CONSTRAINT user_statistic_user
        FOREIGN KEY (user_id)
            REFERENCES "user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: reward_statistic_type (table: reward)
ALTER TABLE reward
    ADD CONSTRAINT reward_statistic_type
        FOREIGN KEY (statistic_type_id)
            REFERENCES statistic_type (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- ==========================================Communication==========================================
-- Table: notification_retry
CREATE TABLE notification_retry
(
    id                   uuid         NOT NULL,
    user_id              uuid         NOT NULL,
    title                varchar(100) NOT NULL,
    message              varchar(255) NOT NULL,
    original_timestamp   timestamp    NOT NULL,
    data                 jsonb        NULL,
    notification_type_id int          NOT NULL,
    CONSTRAINT notification_retry_pk PRIMARY KEY (id)
);

-- Table: notification_type
CREATE TABLE notification_type
(
    id   int          NOT NULL,
    name varchar(255) NOT NULL,
    CONSTRAINT notification_type_pk PRIMARY KEY (id)
);

-- Reference: notification_retry_notification_type (table: notification_retry)
ALTER TABLE notification_retry
    ADD CONSTRAINT notification_retry_notification_type
        FOREIGN KEY (notification_type_id)
            REFERENCES notification_type (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;

-- Reference: notification_retry_user (table: notification_retry)
ALTER TABLE notification_retry
    ADD CONSTRAINT notification_retry_user
        FOREIGN KEY (user_id)
            REFERENCES "user" (id)
            NOT DEFERRABLE
                INITIALLY IMMEDIATE
;
