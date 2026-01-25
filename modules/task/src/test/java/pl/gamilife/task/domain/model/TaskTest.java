package pl.gamilife.task.domain.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.task.domain.model.enums.TaskStatus;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.instancio.Select.field;

class TaskTest {

    private final LocalDateTime now = LocalDateTime.of(2026, 1, 12, 12, 0);

    @Test
    void shouldCreatePrivateTask_whenValidDataIsProvided() {
        // given
        String title = "Test Task";
        String description = "Task Description";
        UUID userId = UUID.randomUUID();
        TaskCategory category = Instancio.create(TaskCategory.class);
        TaskDifficulty difficulty = Instancio.create(TaskDifficulty.class);
        LocalDate deadlineDate = now.toLocalDate().plusDays(1);

        // when
        Task task = Task.builder()
                .title(title)
                .description(description)
                .userId(userId)
                .category(category)
                .difficulty(difficulty)
                .deadlineDate(deadlineDate)
                .buildPrivate(now);

        // then
        assertThat(task).isNotNull();
        assertThat(task.getId()).isNotNull();
        assertThat(task.getTitle()).isEqualTo(title);
        assertThat(task.getUserId()).isEqualTo(userId);
        assertThat(task.getCategory()).isEqualTo(category);
        assertThat(task.getDifficulty()).isEqualTo(difficulty);
        assertThat(task.getDeadlineDate()).isEqualTo(deadlineDate);
        assertThat(task.isGroupTask()).isFalse();
    }

    @Test
    void shouldCreateGroupTask_whenValidDataIsProvided() {
        // given
        String title = "Task";
        String description = "Description";
        TaskCategory category = Instancio.create(TaskCategory.class);
        TaskDifficulty difficulty = Instancio.create(TaskDifficulty.class);
        LocalDate deadlineDate = now.toLocalDate().plusDays(1);

        // when
        Task task = Task.builder()
                .title(title)
                .description(description)
                .category(category)
                .difficulty(difficulty)
                .deadlineDate(deadlineDate)
                .buildForGroupTask(now);

        // then
        assertThat(task).isNotNull();
        assertThat(task.getId()).isNotNull();
        assertThat(task.getCategory()).isEqualTo(category);
        assertThat(task.getDifficulty()).isEqualTo(difficulty);
        assertThat(task.getDeadlineDate()).isEqualTo(deadlineDate);
        assertThat(task.getUserId()).isNull();
        assertThat(task.isGroupTask()).isTrue();
    }

    @Test
    void shouldThrowExceptionDuringCreation_whenBuildingPrivateTaskWithoutUserId() {
        // given
        Task.TaskBuilder builder = Task.builder()
                .title("Task")
                .description("description")
                .category(Instancio.create(TaskCategory.class))
                .difficulty(Instancio.create(TaskDifficulty.class))
                .deadlineDate(now.toLocalDate().plusDays(1));

        // when
        Throwable thrown = catchThrowable(() -> builder.buildPrivate(now));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("User id cannot be null for private task");
    }

    @Test
    void shouldEraseUserId_whenBuildingGroupTaskWithUserId() {
        // given
        UUID userId = UUID.randomUUID();

        // when
        Task task = Task.builder()
                .title("Task")
                .description("description")
                .userId(userId)
                .category(Instancio.create(TaskCategory.class))
                .difficulty(Instancio.create(TaskDifficulty.class))
                .deadlineDate(now.plusDays(10).toLocalDate())
                .buildForGroupTask(now);

        // then
        assertThat(task).isNotNull();
        assertThat(task.getId()).isNotNull();
        assertThat(task.getUserId()).isNull();
    }

    @Test
    void shouldThrowExceptionDuringCreation_whenTooLongTitleProvided() {
        // given
        String longTitle = "a".repeat(201);
        Task task = createTaskForTomorrow();

        // when
        Throwable thrown = catchThrowable(() -> task.setTitle(longTitle));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Title cannot be longer than 200 characters");
    }

    @Test
    void shouldThrowExceptionDuringCreation_whenBlankTitleProvided() {
        // given
        Task task = createTaskForTomorrow();

        // when
        Throwable thrown = catchThrowable(() -> task.setTitle("   "));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Title cannot be null or empty");
    }

    @Test
    void shouldThrowExceptionDuringCreation_whenDeadlineIsInThePast() {
        // given
        LocalDate pastDate = now.toLocalDate().minusDays(1);
        Task.TaskBuilder builder = Task.builder()
                .title("Past Task")
                .userId(UUID.randomUUID())
                .category(Instancio.create(TaskCategory.class))
                .difficulty(Instancio.create(TaskDifficulty.class))
                .deadlineDate(pastDate);

        // when
        Throwable thrown = catchThrowable(() -> builder.buildPrivate(now));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Deadline cannot be in the past");
    }

    @Test
    void shouldThrowExceptionDuringCreation_whenDeadlineTimeIsInThePastOnCurrentDay() {
        // given
        LocalDate today = now.toLocalDate();
        LocalTime pastTime = now.toLocalTime().minusHours(1);
        Task.TaskBuilder builder = Task.builder()
                .title("Past Time Task")
                .userId(UUID.randomUUID())
                .category(Instancio.create(TaskCategory.class))
                .difficulty(Instancio.create(TaskDifficulty.class))
                .deadlineDate(today)
                .deadlineTime(pastTime);

        // when
        Throwable thrown = catchThrowable(() -> builder.buildPrivate(now));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Deadline cannot be in the past");
    }

    @Test
    void shouldCreateTask_whenDeadlineTimeIsInThePastOnDifferentDay() {
        // given
        LocalDate tomorrow = now.toLocalDate().plusDays(1);
        LocalTime pastTime = now.toLocalTime().minusHours(1);

        // when
        Task task = Task.builder()
                .title("Past Time Task")
                .userId(UUID.randomUUID())
                .category(Instancio.create(TaskCategory.class))
                .difficulty(Instancio.create(TaskDifficulty.class))
                .deadlineDate(tomorrow)
                .deadlineTime(pastTime)
                .buildPrivate(now);

        // then
        assertThat(task).isNotNull();
        assertThat(task.getId()).isNotNull();
        assertThat(task.getDeadlineDate()).isEqualTo(tomorrow);
        assertThat(task.getDeadlineTime()).isEqualTo(pastTime);
    }

    @Test
    void shouldMarkTaskAsDone_whenNotAlreadyCompleted() {
        // given
        Task task = createTaskForTomorrow();

        // when
        task.markDone();

        // then
        assertThat(task.getCompletedAt()).isNotNull();
    }

    @Test
    void shouldThrowException_whenMarkingAlreadyCompletedTaskAsDone() {
        // given
        Task task = createTaskForTomorrow();
        task.markDone();

        // when
        Throwable thrown = catchThrowable(task::markDone);

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Task has already been completed");
    }

    @Test
    void shouldMarkTaskAsUndone_whenAlreadyCompleted() {
        // given
        Task task = createTaskForTomorrow();
        task.markDone();

        // when
        task.markUndone();

        // then
        assertThat(task.getCompletedAt()).isNull();
    }

    @Test
    void shouldMarkRewardAsIssued_whenNotAlreadyIssued() {
        // given
        Task task = createTaskForTomorrow();

        // when
        task.markRewardAsIssued();

        // then
        assertThat(task.isRewardIssued()).isTrue();
    }

    @Test
    void shouldKeepRewardAsIssued_whenAlreadyIssued() {
        // given
        Task task = createTaskForTomorrow();
        task.markRewardAsIssued();

        // when
        task.markRewardAsIssued();

        // then
        assertThat(task.isRewardIssued()).isTrue();
    }

    @Test
    void shouldReturnCorrectStatus_whenTaskIsCompleted() {
        // given
        Task task = createTaskForTomorrow();
        task.markDone();

        // when
        TaskStatus status = task.calculateCurrentStatus(now);

        // then
        assertThat(status).isEqualTo(TaskStatus.COMPLETED);
    }

    @Test
    void shouldReturnDeadlineTodayStatus_whenDeadlineIsTodayAndNoTimeSet() {
        // given
        Task task = Instancio.of(Task.class)
                .set(field(Task::getDeadlineDate), now.toLocalDate())
                .set(field(Task::getDeadlineTime), null)
                .set(field(Task::getCompletedAt), null)
                .create();

        // when
        TaskStatus status = task.calculateCurrentStatus(now);

        // then
        assertThat(status).isEqualTo(TaskStatus.DEADLINE_TODAY);
    }

    @Test
    void shouldReturnDeadlineMissedStatus_whenDeadlineWasYesterday() {
        // given
        Task task = Instancio.of(Task.class)
                .set(field(Task::getDeadlineDate), now.toLocalDate())
                .set(field(Task::getDeadlineTime), null)
                .set(field(Task::getCompletedAt), null)
                .create();
        LocalDateTime tomorrow = now.plusDays(1);

        // when
        TaskStatus status = task.calculateCurrentStatus(tomorrow);

        // then
        assertThat(status).isEqualTo(TaskStatus.DEADLINE_MISSED);
    }

    @Test
    void shouldReturnDeadlineMissedStatus_whenDeadlineTimeHasPassedToday() {
        // given
        LocalDateTime taskNow = LocalDateTime.of(2026, 1, 12, 10, 0);
        Task task = Instancio.of(Task.class)
                .set(field(Task::getDeadlineDate), taskNow.toLocalDate())
                .set(field(Task::getDeadlineTime), LocalTime.of(11, 0))
                .set(field(Task::getCompletedAt), null)
                .create();

        LocalDateTime currentDateTime = LocalDateTime.of(2026, 1, 12, 11, 0, 1);

        // when
        TaskStatus status = task.calculateCurrentStatus(currentDateTime);

        // then
        assertThat(status).isEqualTo(TaskStatus.DEADLINE_MISSED);
    }

    @Test
    void shouldReturnDeadlineTodayStatus_whenDeadlineTimeIsLaterToday() {
        // given
        LocalDateTime deadline = LocalDateTime.of(2026, 1, 12, 13, 0);
        Task task = Instancio.of(Task.class)
                .set(field(Task::getDeadlineDate), deadline.toLocalDate())
                .set(field(Task::getDeadlineTime), deadline.toLocalTime())
                .set(field(Task::getCompletedAt), null)
                .create();

        LocalDateTime currentDateTime = LocalDateTime.of(2026, 1, 12, 10, 0);

        // when
        TaskStatus status = task.calculateCurrentStatus(currentDateTime);

        // then
        assertThat(status).isEqualTo(TaskStatus.DEADLINE_TODAY);
    }

    @Test
    void shouldReturnIncompleteStatus_whenDeadlineIsTomorrowAndDeadlineTimeNotSet() {
        // given
        LocalDate tomorrow = LocalDate.of(2026, 1, 13);
        Task task = Instancio.of(Task.class)
                .set(field(Task::getDeadlineDate), tomorrow)
                .set(field(Task::getDeadlineTime), null)
                .set(field(Task::getCompletedAt), null)
                .create();

        LocalDateTime today = LocalDateTime.of(2026, 1, 12, 12, 0);

        // when
        TaskStatus status = task.calculateCurrentStatus(today);

        // then
        assertThat(status).isEqualTo(TaskStatus.INCOMPLETE);
    }

    @Test
    void shouldReturnIncompleteStatus_whenDeadlineIsTomorrowAndDeadlineTimeSet() {
        // given
        LocalDate tomorrow = LocalDate.of(2026, 1, 13);
        LocalTime time = LocalTime.of(12, 0);
        Task task = Instancio.of(Task.class)
                .set(field(Task::getDeadlineDate), tomorrow)
                .set(field(Task::getDeadlineTime), time)
                .set(field(Task::getCompletedAt), null)
                .create();

        LocalDateTime today = LocalDateTime.of(2026, 1, 12, 12, 0);

        // when
        TaskStatus status = task.calculateCurrentStatus(today);

        // then
        assertThat(status).isEqualTo(TaskStatus.INCOMPLETE);
    }

    @Test
    void shouldReturnTrue_whenTaskIsOwnedByUser() {
        // given
        UUID userId = UUID.randomUUID();
        Task task = Instancio.of(Task.class)
                .set(field(Task::getUserId), userId)
                .create();

        // when
        boolean result = task.isOwnedBy(userId);

        // then
        assertThat(result).isTrue();
    }

    @Test
    void shouldReturnFalse_whenTaskIsGroupTaskDuringOwnershipCheck() {
        // given
        UUID userId = UUID.randomUUID();
        Task task = Instancio.of(Task.class)
                .set(field(Task::getDeadlineDate), now.toLocalDate())
                .set(field(Task::getDeadlineTime), null)
                .set(field(Task::getUserId), null)
                .create();

        // when
        boolean result = task.isOwnedBy(userId);

        // then
        assertThat(result).isFalse();
    }

    @Test
    void shouldThrowExceptionDuringOwnershipCheck_whenNoUserIdProvided() {
        // given
        UUID userId = null;
        Task task = Instancio.create(Task.class);

        // when
        Throwable throwable = catchThrowable(() -> task.isOwnedBy(userId));

        // then
        assertThat(throwable)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("User id cannot be null");
    }

    @Test
    void shouldRescheduleDeadline_whenValidDataIsProvided() {
        // given
        Task task = createTaskForTomorrow();
        LocalDate newDate = now.toLocalDate().plusDays(5);
        LocalTime newTime = LocalTime.of(14, 30);

        // when
        task.rescheduleDeadline(newDate, newTime, now);

        // then
        assertThat(task.getDeadlineDate()).isEqualTo(newDate);
        assertThat(task.getDeadlineTime()).isEqualTo(newTime);
    }

    @Test
    void shouldThrowExceptionDuringRescheduling_whenNoNewDateProvided() {
        // given
        Task task = createTaskForTomorrow();
        LocalDate newDate = null;

        // when
        Throwable thrown = catchThrowable(() -> task.rescheduleDeadline(newDate, null, now));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Deadline cannot be null");
    }

    @Test
    void shouldThrowException_whenReschedulingToPastDate() {
        // given
        Task task = createTaskForTomorrow();
        LocalDate pastDate = now.toLocalDate().minusDays(1);

        // when
        Throwable thrown = catchThrowable(() -> task.rescheduleDeadline(pastDate, null, now));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Deadline cannot be in the past");
    }

    @Test
    void shouldSetDescriptionToNull_whenBlankDescriptionIsProvided() {
        // given
        Task task = createTaskForTomorrow();

        // when
        task.setDescription("   ");

        // then
        assertThat(task.getDescription()).isNull();
    }

    @Test
    void shouldThrowExceptionDuringEdition_whenTooLongDescriptionProvided() {
        // given
        Task task = createTaskForTomorrow();
        String longDescription = "a".repeat(501);

        // when
        Throwable thrown = catchThrowable(() -> task.setDescription(longDescription));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Description cannot be longer than 500 characters");
    }

    @Test
    void shouldThrowExceptionDuringEdition_whenCategoryIsNull() {
        // given
        Task task = createTaskForTomorrow();

        // when
        Throwable thrown = catchThrowable(() -> task.setCategory(null));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Category cannot be null");
    }

    @Test
    void shouldThrowExceptionDuringEdition_whenDifficultyIsNull() {
        // given
        Task task = createTaskForTomorrow();

        // when
        Throwable thrown = catchThrowable(() -> task.setDifficulty(null));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Difficulty cannot be null");
    }

    private Task createTaskForTomorrow() {
        return Task.builder()
                .title("Valid Task")
                .userId(UUID.randomUUID())
                .category(Instancio.create(TaskCategory.class))
                .difficulty(Instancio.create(TaskDifficulty.class))
                .deadlineDate(now.toLocalDate().plusDays(1))
                .buildPrivate(now);
    }
}
