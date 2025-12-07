package pl.gamilife.task.application.deletetask;


import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.application.deletehabit.DeleteHabitUseCase;
import pl.gamilife.task.entity.Habit;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.repository.TaskRepository;
import pl.gamilife.task.repository.jpa.HabitRepositoryJpa;

import java.util.UUID;

@Component
public class DeleteTaskUseCaseImpl implements DeleteTaskUseCase {
    private final TaskRepository taskRepository;
    private final HabitRepositoryJpa habitRepository;
    private final DeleteHabitUseCase deleteHabitUseCase;
    private final AuthApi currentUserProvider;

    public DeleteTaskUseCaseImpl(TaskRepository taskRepository, HabitRepositoryJpa habitRepository, DeleteHabitUseCase deleteHabitUseCase, AuthApi currentUserProvider) {
        this.taskRepository = taskRepository;
        this.habitRepository = habitRepository;
        this.deleteHabitUseCase = deleteHabitUseCase;
        this.currentUserProvider = currentUserProvider;
    }

    @Override
    @Transactional
    public void execute(UUID taskId) {

        Habit habit = habitRepository.findHabitByTaskId(taskId).orElse(null);
        if (habit != null) {
            deleteHabitUseCase.execute(habit.getId());
            return;
        }

        Task task = taskRepository
                .findById(taskId)
                .orElseThrow(() -> new TaskNotFoundException(
                        "Task with id " + taskId + " not found!"
                ));

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();
        if (!task.getIsGroupTask() && !currentUserDto.userId().equals(task.getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not authorized to delete task for another user!");
        }
        taskRepository.deleteById(taskId);
    }
}
