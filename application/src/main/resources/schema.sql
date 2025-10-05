DROP TABLE IF EXISTS task_notification CASCADE;
DROP TABLE IF EXISTS task CASCADE;
DROP TABLE IF EXISTS task_category CASCADE;
DROP TABLE IF EXISTS task_difficulty CASCADE;
DROP TABLE IF EXISTS habit CASCADE;

CREATE TABLE habit
(
    habit_id        UUID    NOT NULL,
    cycle_length    BIGINT  NOT NULL,
    current_streak  INTEGER NOT NULL,
    longest_streak  INTEGER NOT NULL,
    created_at      TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    is_accepted     BOOLEAN NOT NULL,
    accepted_date   TIMESTAMP WITHOUT TIME ZONE,
    decline_message VARCHAR(300),
    last_edit       TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    CONSTRAINT pk_habit PRIMARY KEY (habit_id)
);

CREATE TABLE task
(
    task_id                UUID         NOT NULL,
    title                  VARCHAR(200) NOT NULL,
    start_time             TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    end_time               TIMESTAMP WITHOUT TIME ZONE,
    category_id            INTEGER      NOT NULL,
    difficulty_id          INTEGER      NOT NULL,
    user_id                UUID         NOT NULL,
    completed_at           TIMESTAMP WITHOUT TIME ZONE,
    task_habit_id               UUID,
    previous_habit_task_id UUID,
    description            VARCHAR(200),
    CONSTRAINT pk_task PRIMARY KEY (task_id)
);

CREATE TABLE task_category
(
    category_id INTEGER     NOT NULL,
    title       VARCHAR(50) NOT NULL,
    value       INTEGER     NOT NULL,
    CONSTRAINT pk_task_category PRIMARY KEY (category_id)
);

CREATE TABLE task_difficulty
(
    difficulty_id INTEGER     NOT NULL,
    title         VARCHAR(50) NOT NULL,
    value         INTEGER     NOT NULL,
    CONSTRAINT pk_task_difficulty PRIMARY KEY (difficulty_id)
);

CREATE TABLE task_notification
(
    id        INTEGER NOT NULL,
    send_date TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    task_id   UUID,
    CONSTRAINT pk_task_notification PRIMARY KEY (id)
);

ALTER TABLE task
    ADD CONSTRAINT uc_task_previous_habit_task UNIQUE (previous_habit_task_id);

ALTER TABLE task_notification
    ADD CONSTRAINT FK_TASK_NOTIFICATION_ON_TASK FOREIGN KEY (task_id) REFERENCES task (task_id);

ALTER TABLE task
    ADD CONSTRAINT FK_TASK_ON_CATEGORY FOREIGN KEY (category_id) REFERENCES task_category (category_id);

ALTER TABLE task
    ADD CONSTRAINT FK_TASK_ON_DIFFICULTY FOREIGN KEY (difficulty_id) REFERENCES task_difficulty (difficulty_id);

ALTER TABLE task
    ADD CONSTRAINT FK_TASK_ON_HABIT FOREIGN KEY (task_habit_id) REFERENCES habit (habit_id);

ALTER TABLE task
    ADD CONSTRAINT FK_TASK_ON_PREVIOUS_HABIT_TASK FOREIGN KEY (previous_habit_task_id) REFERENCES task (task_id);
