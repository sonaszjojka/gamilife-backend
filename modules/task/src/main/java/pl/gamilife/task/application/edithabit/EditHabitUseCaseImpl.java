package pl.gamilife.task.application.edithabit;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.task.controllers.request.EditHabitRequest;
import pl.gamilife.task.controllers.response.EditHabitResponse;
import pl.gamilife.task.entity.Habit;
import pl.gamilife.task.exception.domain.HabitNotFoundException;
import pl.gamilife.task.repository.HabitRepository;

import java.util.UUID;

@Component
@AllArgsConstructor
public class EditHabitUseCaseImpl implements EditHabitUseCase {

    private final HabitRepository habitRepository;

    @Override
    @Transactional
    public EditHabitResponse execute(EditHabitRequest request, UUID taskId) {
        Habit habit = habitRepository.findHabitByTaskId(taskId)
                .orElseThrow(() -> new HabitNotFoundException(
                        "Habit for taskId " + taskId + " not found!"
                ));

        habit.setCycleLength(request.cycleLength());

        if (request.finished()) {
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
