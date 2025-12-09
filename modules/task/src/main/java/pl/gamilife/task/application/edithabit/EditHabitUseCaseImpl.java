package pl.gamilife.task.application.edithabit;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.task.domain.exception.domain.HabitNotFoundException;
import pl.gamilife.task.domain.model.Habit;
import pl.gamilife.task.domain.port.repository.HabitRepository;
import pl.gamilife.task.infrastructure.web.request.EditHabitRequest;
import pl.gamilife.task.infrastructure.web.response.EditHabitResponse;

import java.util.UUID;

@Component
@AllArgsConstructor
public class EditHabitUseCaseImpl implements EditHabitUseCase {

    private final HabitRepository habitRepository;

    @Override
    @Transactional
    public EditHabitResponse execute(EditHabitCommand cmd) {
        Habit habit = habitRepository.findHabitByTaskId(cmd.taskId())
                .orElseThrow(() -> new HabitNotFoundException(
                        "Habit for taskId " + cmd.taskId() + " not found!"
                ));

        habit.setCycleLength(cmd.cycleLength());

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
