package pl.gamilife.task.application.edithabit;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.task.domain.exception.domain.HabitNotFoundException;
import pl.gamilife.task.domain.exception.domain.TaskCategoryNotFoundException;
import pl.gamilife.task.domain.exception.domain.TaskDifficultyNotFoundException;
import pl.gamilife.task.domain.model.Habit;
import pl.gamilife.task.domain.model.TaskCategory;
import pl.gamilife.task.domain.model.TaskDifficulty;
import pl.gamilife.task.domain.port.context.UserContext;
import pl.gamilife.task.domain.port.repository.HabitRepository;
import pl.gamilife.task.domain.port.repository.TaskCategoryRepository;
import pl.gamilife.task.domain.port.repository.TaskDifficultyRepository;
import pl.gamilife.task.infrastructure.web.response.EditHabitResponse;

import java.time.LocalDate;

@Component
@AllArgsConstructor
public class EditHabitUseCaseImpl implements EditHabitUseCase {

    private final TaskCategoryRepository taskCategoryRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;
    private final HabitRepository habitRepository;
    private final UserContext userContext;

    @Override
    @Transactional
    public EditHabitResponse execute(EditHabitCommand cmd) {
        Habit habit = habitRepository.findById(cmd.habitId())
                .orElseThrow(() -> new HabitNotFoundException(
                        "Habit for habitId " + cmd.habitId() + " not found!"
                ));
        LocalDate currentUserDate = userContext.getCurrentUserDate(cmd.userId());

        if (cmd.title() != null) {
            habit.setTitle(cmd.title());
        }

        // TODO: think how to delete
        if (cmd.description() != null) {
            habit.setDescription(cmd.description());
        }

        if (cmd.categoryId() != null) {
            TaskCategory taskCategory = taskCategoryRepository
                    .findById(cmd.categoryId())
                    .orElseThrow(() -> new TaskCategoryNotFoundException(String.format(
                            "Category with id %s not found!",
                            cmd.categoryId()
                    )));
            habit.setCategory(taskCategory);
        }

        if (cmd.difficultyId() != null) {
            TaskDifficulty taskDifficulty = taskDifficultyRepository
                    .findById(cmd.difficultyId())
                    .orElseThrow(() -> new TaskDifficultyNotFoundException(String.format(
                            "Task difficulty with id %s not found!",
                            cmd.difficultyId()
                    )));
            habit.setDifficulty(taskDifficulty);
        }

        if (cmd.cycleLength() != null) {
            habit.editCycleLength(cmd.cycleLength(), currentUserDate);
        }

        if (cmd.iterationCompleted() != null && cmd.iterationCompleted()) {
            habit.completeIteration(currentUserDate);
        }

        if (cmd.finished() != null && cmd.finished()) {
            habit.finish();
        }

        return buildResponse(habitRepository.save(habit));
    }

    public EditHabitResponse buildResponse(Habit habit) {
        return new EditHabitResponse(
                habit.getId(),
                habit.getCycleLength(),
                habit.getCurrentStreak(),
                habit.getLongestStreak(),
                habit.getFinishedAt(),
                habit.getUpdatedAt(),
                habit.getCreatedAt()
        );
    }
}
