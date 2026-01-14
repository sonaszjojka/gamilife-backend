package pl.gamilife.task.domain.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;

import java.time.LocalDate;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.instancio.Select.field;

class HabitTest {

    private final LocalDate today = LocalDate.of(2026, 1, 12);

    @Test
    void shouldCreateHabit_whenValidDataIsProvided() {
        // given
        String title = "Daily Exercise";
        String description = "Get fit";
        UUID userId = UUID.randomUUID();
        TaskCategory category = Instancio.create(TaskCategory.class);
        TaskDifficulty difficulty = Instancio.create(TaskDifficulty.class);
        int cycleLength = 1;

        // when
        Habit habit = Habit.create(title, description, userId, category, difficulty, cycleLength, today);

        // then
        assertThat(habit).isNotNull();
        assertThat(habit.getTitle()).isEqualTo(title);
        assertThat(habit.getCycleLength()).isEqualTo(cycleLength);
        assertThat(habit.getCurrentDeadline()).isEqualTo(today);
        assertThat(habit.getCurrentStreak()).isZero();
    }

    @Test
    void shouldThrowExceptionDuringCreation_whenCycleLengthIsInvalid() {
        // given
        int invalidCycleLength = 0;

        // when
        Throwable thrown = catchThrowable(() -> Habit.create(
                "Title",
                null,
                UUID.randomUUID(),
                Instancio.create(TaskCategory.class),
                Instancio.create(TaskDifficulty.class),
                invalidCycleLength,
                today
        ));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Cycle length must be a positive integer");
    }

    @Test
    void shouldThrowExceptionDuringCreation_whenNoUserIdProvided() {
        // given
        UUID userId = null;

        // when
        Throwable thrown = catchThrowable(() -> Habit.create(
                "Title",
                null,
                userId,
                Instancio.create(TaskCategory.class),
                Instancio.create(TaskDifficulty.class),
                1,
                today
        ));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("User id cannot be null");
    }

    @Test
    void shouldThrowExceptionDuringCreation_whenNoTitleProvided() {
        // given
        String title = null;

        // when
        Throwable thrown = catchThrowable(() -> Habit.create(
                title,
                null,
                UUID.randomUUID(),
                Instancio.create(TaskCategory.class),
                Instancio.create(TaskDifficulty.class),
                1,
                today
        ));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Title cannot be null or empty");
    }

    @Test
    void shouldThrowExceptionDuringCreation_whenBlankTitleProvided() {
        // given
        String title = "   ";

        // when
        Throwable thrown = catchThrowable(() -> Habit.create(
                title,
                null,
                UUID.randomUUID(),
                Instancio.create(TaskCategory.class),
                Instancio.create(TaskDifficulty.class),
                1,
                today
        ));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Title cannot be null or empty");
    }

    @Test
    void shouldThrowExceptionDuringCreation_whenTooLongTitleProvided() {
        // given
        String title = "a".repeat(201);

        // when
        Throwable thrown = catchThrowable(() -> Habit.create(
                title,
                null,
                UUID.randomUUID(),
                Instancio.create(TaskCategory.class),
                Instancio.create(TaskDifficulty.class),
                1,
                today
        ));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Title cannot be longer than 200 characters");
    }

    @Test
    void shouldCreateHabit_whenNoDescriptionProvided() {
        // given
        String description = null;

        // when
        Habit habit = Habit.create(
                "title",
                description,
                UUID.randomUUID(),
                Instancio.create(TaskCategory.class),
                Instancio.create(TaskDifficulty.class),
                1,
                today
        );

        // then
        assertThat(habit).isNotNull();
        assertThat(habit.getId()).isNotNull();
    }

    @Test
    void shouldThrowExceptionDuringCreation_whenTooLongDescriptionProvided() {
        // given
        String description = "a".repeat(501);

        // when
        Throwable thrown = catchThrowable(() -> Habit.create(
                "title",
                description,
                UUID.randomUUID(),
                Instancio.create(TaskCategory.class),
                Instancio.create(TaskDifficulty.class),
                1,
                today
        ));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Description cannot be longer than 500 characters");
    }

    @Test
    void shouldThrowExceptionDuringCreation_whenNoCategoryProvided() {
        // given
        TaskCategory category = null;

        // when
        Throwable thrown = catchThrowable(() -> Habit.create(
                "title",
                null,
                UUID.randomUUID(),
                category,
                Instancio.create(TaskDifficulty.class),
                1,
                today
        ));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Category cannot be null");
    }

    @Test
    void shouldThrowExceptionDuringCreation_whenNoDifficultyProvided() {
        // given
        TaskDifficulty difficulty = null;

        // when
        Throwable thrown = catchThrowable(() -> Habit.create(
                "title",
                null,
                UUID.randomUUID(),
                Instancio.create(TaskCategory.class),
                difficulty,
                1,
                today
        ));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Difficulty cannot be null");
    }

    @Test
    void shouldCanBeWorkedOnReturnTrue_whenIterationNotCompletedAndNotDead() {
        // given
        Habit habit = createDailyHabit();

        // when
        boolean canBeWorkedOn = habit.canBeWorkedOn(today);

        // then
        assertThat(canBeWorkedOn).isTrue();
    }

    @Test
    void shouldCompleteIteration_whenValidDateProvided() {
        // given
        Habit habit = createDailyHabit();
        LocalDate nextExpectedDeadline = today.plusDays(1);

        // when
        habit.completeIteration(today);

        // then
        assertThat(habit.getCurrentStreak()).isEqualTo(1);
        assertThat(habit.getLastCompletedDate()).isEqualTo(today);
        assertThat(habit.getCurrentDeadline()).isEqualTo(nextExpectedDeadline);
    }

    @Test
    void shouldIncrementLongestStreak_whenCurrentStreakExceedsIt() {
        // given
        Habit habit = createDailyHabit();

        // when
        habit.completeIteration(today);
        habit.completeIteration(today.plusDays(1));

        // then
        assertThat(habit.getCurrentStreak()).isEqualTo(2);
        assertThat(habit.getLongestStreak()).isEqualTo(2);
    }

    @Test
    void shouldNotIncrementLongestStreak_whenCurrentStreakDoesNotExceedIt() {
        // given
        Habit habit = Instancio.of(Habit.class)
                .set(field(Habit::getCurrentStreak), 5)
                .set(field(Habit::getLongestStreak), 5)
                .set(field(Habit::getCurrentDeadline), today.minusDays(2))
                .create();
        habit.resurrectHabit(today);

        // when
        habit.completeIteration(today);

        // then
        assertThat(habit.getCurrentStreak()).isEqualTo(1);
        assertThat(habit.getLongestStreak()).isEqualTo(5);
    }

    @Test
    void shouldThrowExceptionDuringCompletion_whenIterationAlreadyCompleted() {
        // given
        Habit habit = createDailyHabit();
        habit.completeIteration(today);

        // when
        Throwable thrown = catchThrowable(() -> habit.completeIteration(today));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessageContaining("Current habit iteration already completed");
    }

    @Test
    void shouldBeDead_whenCurrentDateIsAfterDeadline() {
        // given
        Habit habit = createDailyHabit();
        LocalDate tomorrow = today.plusDays(1);

        // when
        boolean isDead = habit.isHabitDead(tomorrow);

        // then
        assertThat(isDead).isTrue();
    }

    @Test
    void shouldThrowExceptionDuringCompletion_whenHabitIsDead() {
        // given
        Habit habit = createDailyHabit();
        LocalDate tomorrow = today.plusDays(1);

        // when
        Throwable thrown = catchThrowable(() -> habit.completeIteration(tomorrow));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Cannot continue a dead habit. Resurrect it first.");
    }

    @Test
    void shouldEditCycleLength_whenValidNewCycleLengthProvided() {
        // given
        Habit habit = createDailyHabit();
        int newLength = 7;

        // when
        habit.editCycleLength(newLength, today);

        // then
        assertThat(habit.getCycleLength()).isEqualTo(7);
        assertThat(habit.getCurrentDeadline()).isEqualTo(today.minusDays(1).plusDays(7));
    }

    @Test
    void shouldThrowExceptionDuringEditCycleLength_whenNegativeNewCycleLengthProvided() {
        // given
        Habit habit = createDailyHabit();
        int newLength = -1;

        // when
        Throwable throwable = catchThrowable(() -> habit.editCycleLength(newLength, today));

        // then
        assertThat(throwable)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Cycle length must be a positive integer");
    }

    @Test
    void shouldThrowExceptionDuringEditCycleLength_whenNewCycleLengthIsZero() {
        // given
        Habit habit = createDailyHabit();
        int newLength = 0;

        // when
        Throwable throwable = catchThrowable(() -> habit.editCycleLength(newLength, today));

        // then
        assertThat(throwable)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Cycle length must be a positive integer");
    }

    @Test
    void shouldThrowExceptionDuringEditCycle_whenNewDeadlineIsInThePast() {
        // given
        Habit habit = Instancio.of(Habit.class)
                .set(field(Habit::getCycleLength), 10)
                .set(field(Habit::getCurrentDeadline), today.minusDays(5))
                .create();

        // when
        Throwable thrown = catchThrowable(() -> habit.editCycleLength(2, today));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessageContaining("Your new deadline would be in the past");
    }

    @Test
    void shouldResurrectHabit_whenItIsDead() {
        // given
        Habit habit = createDailyHabit();
        habit.completeIteration(today);

        LocalDate weekLater = today.plusDays(8);

        // when
        habit.resurrectHabit(weekLater);

        // then
        assertThat(habit.isHabitDead(weekLater)).isFalse();
        assertThat(habit.getCurrentStreak()).isZero();
        assertThat(habit.getCurrentDeadline()).isEqualTo(weekLater);
    }

    @Test
    void shouldThrowExceptionDuringResurrect_whenHabitIsNotDead() {
        // given
        Habit habit = createDailyHabit();

        // when
        Throwable thrown = catchThrowable(() -> habit.resurrectHabit(today));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Current habit iteration has not yet ended.");
    }

    private Habit createDailyHabit() {
        return Habit.create(
                "Exercise",
                "Daily",
                UUID.randomUUID(),
                Instancio.create(TaskCategory.class),
                Instancio.create(TaskDifficulty.class),
                1,
                today
        );
    }
}
