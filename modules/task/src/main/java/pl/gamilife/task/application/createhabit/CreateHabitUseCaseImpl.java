package pl.gamilife.task.application.createhabit;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.task.domain.exception.domain.TaskCategoryNotFoundException;
import pl.gamilife.task.domain.exception.domain.TaskDifficultyNotFoundException;
import pl.gamilife.task.domain.model.Habit;
import pl.gamilife.task.domain.model.TaskCategory;
import pl.gamilife.task.domain.model.TaskDifficulty;
import pl.gamilife.task.domain.port.context.UserContext;
import pl.gamilife.task.domain.port.repository.HabitRepository;
import pl.gamilife.task.domain.port.repository.TaskCategoryRepository;
import pl.gamilife.task.domain.port.repository.TaskDifficultyRepository;

import java.time.LocalDate;

@Service
@AllArgsConstructor
public class CreateHabitUseCaseImpl implements CreateHabitUseCase {

    private final HabitRepository habitRepository;
    private final UserContext userContext;
    private final TaskCategoryRepository taskCategoryRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;

    @Override
    public CreateHabitResult execute(CreateHabitCommand cmd) {
        TaskCategory taskCategory = taskCategoryRepository
                .findById(cmd.categoryId())
                .orElseThrow(() -> new TaskCategoryNotFoundException(String.format(
                        "Category with id %s not found!",
                        cmd.categoryId()
                )));

        TaskDifficulty taskDifficulty = taskDifficultyRepository
                .findById(cmd.difficultyId())
                .orElseThrow(() -> new TaskDifficultyNotFoundException(String.format(
                        "Task difficulty with id %s not found!",
                        cmd.difficultyId()
                )));

        LocalDate currentUserDate = userContext.getCurrentUserDate(cmd.userId());

        Habit habit = Habit.create(
                cmd.title(),
                cmd.description(),
                cmd.userId(),
                taskCategory,
                taskDifficulty,
                cmd.cycleLength(),
                currentUserDate
        );
        habitRepository.save(habit);

        return buildResponse(habit);
    }

    private CreateHabitResult buildResponse(Habit habit) {
        return new CreateHabitResult(
                habit.getId(),
                habit.getCycleLength(),
                habit.getCurrentDeadline(),
                habit.getCurrentStreak(),
                habit.getLongestStreak(),
                habit.getCreatedAt(),
                habit.getUpdatedAt()
        );
    }
}
