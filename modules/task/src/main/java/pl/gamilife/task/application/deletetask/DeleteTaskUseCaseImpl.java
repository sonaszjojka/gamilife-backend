package pl.gamilife.task.application.deletetask;


import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.enums.ActivityType;
import pl.gamilife.shared.kernel.event.ActivityDeletionRequestedEvent;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.port.repository.TaskRepository;

@Service
@Transactional
@AllArgsConstructor
public class DeleteTaskUseCaseImpl implements DeleteTaskUseCase {
    private final TaskRepository taskRepository;
    private final ApplicationEventPublisher eventPublisher;

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

        eventPublisher.publishEvent(new ActivityDeletionRequestedEvent(ActivityType.TASK, task.getId()));
        taskRepository.deleteById(cmd.taskId());

        return null;
    }
}
