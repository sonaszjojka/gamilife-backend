package pl.gamilife.pomodoro.application.createpomodoroitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.pomodoro.domain.exception.PomodoroItemAlreadyExists;
import pl.gamilife.pomodoro.domain.model.PomodoroItem;
import pl.gamilife.pomodoro.domain.model.projection.PomodoroHabit;
import pl.gamilife.pomodoro.domain.model.projection.PomodoroTask;
import pl.gamilife.pomodoro.domain.port.context.TaskContext;
import pl.gamilife.pomodoro.domain.port.repository.PomodoroItemRepository;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

@Service
@Transactional
@AllArgsConstructor
public class CreatePomodoroItemUseCaseImpl implements CreatePomodoroItemUseCase {

    private final PomodoroItemRepository pomodoroRepository;
    private final TaskContext taskContext;

    @Override
    public CreatePomodoroItemResult execute(CreatePomodoroItemCommand cmd) {
        return cmd.taskId() == null ?
                createPomodoroItemForHabit(cmd) :
                createPomodoroItemForTask(cmd);
    }

    private CreatePomodoroItemResult createPomodoroItemForTask(CreatePomodoroItemCommand cmd) {
        if (pomodoroRepository.existsByTaskId(cmd.taskId())) {
            throw new PomodoroItemAlreadyExists("Task with id:" + cmd.taskId() + " already has an attached pomodoro item");
        }

        PomodoroTask pomodoroTask = taskContext.findTaskById(cmd.taskId());
        if (!pomodoroTask.userId().equals(cmd.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not owner of the task with id: " + cmd.taskId());
        }

        PomodoroItem pomodoroItem = PomodoroItem.createForTask(cmd.cyclesRequired(), cmd.taskId());

        pomodoroRepository.save(pomodoroItem);

        return buildResponse(pomodoroItem, true);
    }

    private CreatePomodoroItemResult createPomodoroItemForHabit(CreatePomodoroItemCommand cmd) {
        if (pomodoroRepository.existsByHabitId(cmd.habitId())) {
            throw new PomodoroItemAlreadyExists("Habit with id:" + cmd.habitId() + " already has an attached pomodoro item");
        }

        PomodoroHabit pomodoroHabit = taskContext.findHabitById(cmd.habitId(), cmd.userId(), cmd.zoneId());
        if (!pomodoroHabit.userId().equals(cmd.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not the owner of the habit with id: " + cmd.habitId());
        }

        PomodoroItem pomodoroItem = PomodoroItem.createForHabit(cmd.cyclesRequired(), cmd.habitId());

        pomodoroRepository.save(pomodoroItem);

        return buildResponse(pomodoroItem, pomodoroHabit.canBeWorkedOn());
    }

    private CreatePomodoroItemResult buildResponse(PomodoroItem pomodoroItem, boolean canBeWorkedOn) {
        return new CreatePomodoroItemResult(
                pomodoroItem.getId(),
                pomodoroItem.getCyclesRequired(),
                pomodoroItem.getCyclesCompleted(),
                pomodoroItem.getTaskId(),
                pomodoroItem.getHabitId(),
                pomodoroItem.getCreatedAt(),
                canBeWorkedOn
        );
    }
}
