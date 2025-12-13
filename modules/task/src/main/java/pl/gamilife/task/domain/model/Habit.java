package pl.gamilife.task.domain.model;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.UUID;

@Getter
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "habit")
public class Habit extends BaseEntity {

    @Column(name = "title", length = 200, nullable = false)
    private String title;

    @Column(name = "description", length = 500)
    private String description;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "category_id", nullable = false)
    private Integer categoryId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "category_id", nullable = false, insertable = false, updatable = false)
    private TaskCategory category;

    @Column(name = "difficulty_id", nullable = false)
    private Integer difficultyId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "difficulty_id", nullable = false, insertable = false, updatable = false)
    private TaskDifficulty difficulty;

    @Column(name = "cycle_length", nullable = false)
    private Integer cycleLength;

    @Column(name = "current_deadline", nullable = false)
    private LocalDate currentDeadline;

    @Column(name = "last_completed_date")
    private LocalDate lastCompletedDate;

    @Column(name = "current_streak", nullable = false)
    private Integer currentStreak = 0;

    @Column(name = "longest_streak", nullable = false)
    private Integer longestStreak = 0;

    private Habit(String title, String description, UUID userId, TaskCategory category, TaskDifficulty difficulty, int cycleLength, LocalDate currentUserDate) {
        setTitle(title);
        setDescription(description);
        setUserId(userId);
        setCategory(category);
        setDifficulty(difficulty);
        initHabitDates(cycleLength, currentUserDate);
    }

    public static Habit create(String title, String description, UUID userId, TaskCategory category, TaskDifficulty difficulty, int cycleLength, LocalDate currentUserDate) {
        return new Habit(title, description, userId, category, difficulty, cycleLength, currentUserDate);
    }

    private void initHabitDates(int cycleLength, LocalDate currentUserDate) {
        if (cycleLength <= 0) {
            throw new DomainValidationException("Cycle length must be a positive integer");
        }

        this.cycleLength = cycleLength;
        // Subtract 1 to start a habit right now
        this.currentDeadline = currentUserDate.plusDays(cycleLength - 1L);
    }

    private void setUserId(UUID userId) {
        if (userId == null) {
            throw new DomainValidationException("User id cannot be null");
        }

        this.userId = userId;
    }

    public void setTitle(String title) {
        if (title == null || title.isBlank()) {
            throw new DomainValidationException("Title cannot be null or empty");
        }

        if (title.length() > 200) {
            throw new DomainValidationException("Title cannot be longer than 200 characters");
        }

        this.title = title;
    }

    public void setDescription(String description) {
        if (description == null || description.isBlank()) {
            return;
        }

        if (description.length() > 500) {
            throw new DomainValidationException("Description cannot be longer than 500 characters");
        }

        this.description = description;
    }

    public void setCategory(TaskCategory taskCategory) {
        if (taskCategory == null) {
            throw new DomainValidationException("Category cannot be null");
        }

        this.category = taskCategory;
        this.categoryId = taskCategory.getId();
    }

    public void setDifficulty(TaskDifficulty taskDifficulty) {
        if (taskDifficulty == null) {
            throw new DomainValidationException("Difficulty cannot be null");
        }

        this.difficulty = taskDifficulty;
        this.difficultyId = taskDifficulty.getId();
    }

    public boolean checkIfCanBeWorkedOn(LocalDate currentUserDate) {
        return !currentUserDate.isAfter(getPreviousDeadline());
    }

    public void completeIteration(LocalDate currentUserDate) {
        LocalDate previousDeadline = getPreviousDeadline();
        if (checkIfCanBeWorkedOn(currentUserDate)) {
            throw new DomainValidationException(
                    String.format("Current habit iteration already completed. Try again after %s", previousDeadline)
            );
        }

        if (isHabitDead(currentUserDate)) {
            throw new DomainValidationException("Cannot continue a dead habit. Resurrect it first.");
        }

        incrementCurrentStreak();
        lastCompletedDate = currentUserDate;
        currentDeadline = currentDeadline.plusDays(cycleLength);
    }

    public void editCycleLength(int newCycleLength, LocalDate currentUserDate) {
        if (newCycleLength <= 0) {
            throw new DomainValidationException("Cycle length must be a positive integer");
        }

        LocalDate previousDeadline = getPreviousDeadline();
        LocalDate newDeadline = previousDeadline.plusDays(newCycleLength);
        if (currentUserDate.isAfter(newDeadline)) {
            throw new DomainValidationException(String.format(
                    "Your new deadline would be in the past. New cycle count must be at least %s days",
                    ChronoUnit.DAYS.between(previousDeadline, currentUserDate)
            ));
        }

        this.cycleLength = newCycleLength;
        this.currentDeadline = newDeadline;
    }

    public boolean isHabitDead(LocalDate currentUserDate) {
        return currentDeadline.isBefore(currentUserDate);
    }

    private LocalDate getPreviousDeadline() {
        return currentDeadline.minusDays(cycleLength);
    }

    public void resurrectHabit(LocalDate currentUserDate) {
        if (!isHabitDead(currentUserDate)) {
            throw new DomainValidationException("Current habit iteration has not yet ended.");
        }

        long daysPastDeadline = ChronoUnit.DAYS.between(currentDeadline, currentUserDate);
        long cyclesToAdd = (daysPastDeadline + cycleLength - 1) / cycleLength;
        currentDeadline = currentDeadline.plusDays(cyclesToAdd * cycleLength);
        this.currentStreak = 0;
    }

    private void incrementCurrentStreak() {
        this.currentStreak++;
        if (currentStreak > longestStreak) {
            longestStreak = currentStreak;
        }
    }
}
