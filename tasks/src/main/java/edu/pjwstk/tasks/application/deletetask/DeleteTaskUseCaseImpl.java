package edu.pjwstk.tasks.application.deletetask;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.exception.TaskNotFoundException;
import edu.pjwstk.tasks.exception.UnauthorizedTaskAccessException;
import edu.pjwstk.tasks.repository.TaskRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Component
public class DeleteTaskUseCaseImpl implements DeleteTaskUseCase {
    private final TaskRepository taskRepository;
    private final AuthApi currentUserProvider;

    public DeleteTaskUseCaseImpl(TaskRepository taskRepository, AuthApi currentUserProvider) {
        this.taskRepository = taskRepository;
        this.currentUserProvider = currentUserProvider;
    }

    @Override
    @Transactional
    public void execute(UUID taskId) {

        Task task = taskRepository
                .findById(taskId)
                .orElseThrow(() -> new TaskNotFoundException(
                        "Task with id " + taskId + " not found!"
                ));

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser().orElseThrow();
        if (!currentUserDto.userId().equals(task.getUserId())) {
            throw new UnauthorizedTaskAccessException("User is not authorized to delete task for another user!");
        }
        taskRepository.deleteById(taskId);
    }
}
