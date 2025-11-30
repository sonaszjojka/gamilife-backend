package edu.pjwstk.tasks.application.deletetask;


import edu.pjwstk.tasks.application.deletehabit.DeleteHabitUseCase;
import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.ResourceOwnerPrivilegesRequiredException;
import edu.pjwstk.core.exception.common.domain.TaskNotFoundException;
import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.repository.HabitRepository;
import edu.pjwstk.tasks.repository.TaskRepository;
import edu.pjwstk.tasks.repository.jpa.HabitRepositoryJpa;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

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

       Habit habit= habitRepository.findHabitByTaskId(taskId).orElseThrow(()-> new TaskNotFoundException("Task with id " + taskId + " does not exist."));
       if (habit!=null) {
           deleteHabitUseCase.execute(habit.getId());
           return;
       }

        Task task = taskRepository
                .findById(taskId)
                .orElseThrow(() -> new TaskNotFoundException(
                        "Task with id " + taskId + " not found!"
                ));

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();
        if (!task.getIsGroupTask()&&!currentUserDto.userId().equals(task.getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not authorized to delete task for another user!");
        }
        taskRepository.deleteById(taskId);
    }
}
