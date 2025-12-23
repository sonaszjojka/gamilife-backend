package pl.gamilife.task.application.deletetask;


import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.port.repository.TaskRepository;

// TODO: split for standard and group task
@Service
@Transactional
@AllArgsConstructor
public class DeleteTaskUseCaseImpl implements DeleteTaskUseCase {
    private final TaskRepository taskRepository;

    @Override
    public Void execute(DeleteTaskCommand cmd) {
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
