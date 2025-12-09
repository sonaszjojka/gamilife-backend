package pl.gamilife.task.application.deletetask;


import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.application.deletehabit.DeleteHabitUseCase;
import pl.gamilife.task.domain.model.Habit;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.port.repository.TaskRepository;
import pl.gamilife.task.infrastructure.persistence.jpa.JpaHabitRepository;

// TODO: split for standard and group task
@Service
@Transactional
@AllArgsConstructor
public class DeleteTaskUseCaseImpl implements DeleteTaskUseCase {
    private final TaskRepository taskRepository;
    private final JpaHabitRepository habitRepository;
    private final DeleteHabitUseCase deleteHabitUseCase;

    @Override
    public Void execute(DeleteTaskCommand cmd) {

        Habit habit = habitRepository.findHabitByTaskId(cmd.taskId()).orElse(null);
        if (habit != null) {
            deleteHabitUseCase.execute(habit.getId());
            return null;
        }

        Task task = taskRepository
                .findById(cmd.taskId())
                .orElseThrow(() -> new TaskNotFoundException(
                        "Task with id " + cmd + " not found!"
                ));

        if (!task.isGroupTask() && !cmd.userId().equals(task.getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not authorized to delete task for another user!");
        }

        taskRepository.deleteById(cmd.taskId());

        return null;
    }
}
