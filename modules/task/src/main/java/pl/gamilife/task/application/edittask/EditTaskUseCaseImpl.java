package pl.gamilife.task.application.edittask;

import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.event.TaskCompletedEvent;
import pl.gamilife.shared.kernel.event.TaskUndoneEvent;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.domain.exception.domain.TaskCategoryNotFoundException;
import pl.gamilife.task.domain.exception.domain.TaskDifficultyNotFoundException;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.model.TaskCategory;
import pl.gamilife.task.domain.model.TaskDifficulty;
import pl.gamilife.task.domain.port.context.UserContext;
import pl.gamilife.task.domain.port.repository.TaskCategoryRepository;
import pl.gamilife.task.domain.port.repository.TaskDifficultyRepository;
import pl.gamilife.task.domain.port.repository.TaskRepository;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Objects;

@Component
@AllArgsConstructor
public class EditTaskUseCaseImpl implements EditTaskUseCase {

    private final TaskRepository taskRepository;
    private final TaskDifficultyRepository taskDifficultyRepository;
    private final TaskCategoryRepository taskCategoryRepository;
    private final UserContext userContext;
    private final ApplicationEventPublisher eventPublisher;

    @Override
    @Transactional
    public EditTaskResult execute(EditTaskCommand cmd) {
        Task task = taskRepository.findById(cmd.taskId())
                .orElseThrow(() -> new TaskNotFoundException("Task with id " + cmd.taskId() + " not found."));

        if (!task.isGroupTask() && !cmd.userId().equals(task.getUserId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not authorized to edit task for another user!");
        }

        if (cmd.title() != null) {
            task.setTitle(cmd.title());
        }

        boolean rescheduled = rescheduleDeadline(task, cmd);
        if (rescheduled) {
            // TODO: remove notifications
        }

        if (Boolean.TRUE.equals(cmd.removeDescription()) && cmd.description() == null) {
            task.setDescription(null);
        } else {
            task.setDescription(cmd.description());
        }

        if (Boolean.TRUE.equals(cmd.completed())) {
            task.markDone();
            eventPublisher.publishEvent(new TaskCompletedEvent(cmd.userId(), task.isRewardIssued()));
            task.markRewardAsIssued();
        } else if (Boolean.FALSE.equals(cmd.completed())) {
            task.markUndone();
            eventPublisher.publishEvent(new TaskUndoneEvent(cmd.userId(), task.getId()));
        }

        if (cmd.categoryId() != null && !Objects.equals(task.getCategoryId(), cmd.categoryId())) {
            TaskCategory taskCategory = taskCategoryRepository
                    .findById(cmd.categoryId())
                    .orElseThrow(() -> new TaskCategoryNotFoundException("Category with id " + cmd.categoryId() + " not found!"));
            task.setCategory(taskCategory);
        }

        if (cmd.difficultyId() != null && !Objects.equals(task.getDifficultyId(), cmd.difficultyId())) {
            TaskDifficulty taskDifficulty = taskDifficultyRepository
                    .findById(cmd.difficultyId())
                    .orElseThrow(() -> new TaskDifficultyNotFoundException("Task difficulty with id " + cmd.difficultyId() + " not found!"));
            task.setDifficulty(taskDifficulty);
        }

        return buildResponse(taskRepository.save(task));
    }

    private boolean rescheduleDeadline(Task task, EditTaskCommand cmd) {
        boolean dateChanged = cmd.deadlineDate() != null;
        boolean timeChanged = cmd.deadlineTime() != null;
        boolean timeRemoved = Boolean.TRUE.equals(cmd.removeDeadlineTime()) && task.getDeadlineTime() != null;

        if (!dateChanged && !timeChanged && !timeRemoved) {
            return false;
        }

        LocalDate newDate = dateChanged
                ? cmd.deadlineDate()
                : task.getDeadlineDate();
        LocalTime newTime = task.getDeadlineTime();
        if (timeChanged) {
            newTime = cmd.deadlineTime();
        } else if (timeRemoved) {
            newTime = null;
        }

        ZoneId zoneId = cmd.zoneId() == null ? userContext.getCurrentUserTimezone(cmd.userId()) : cmd.zoneId();
        LocalDateTime currentUserDateTime = LocalDateTime.now(zoneId);

        task.rescheduleDeadline(newDate, newTime, currentUserDateTime);

        return true;
    }

    private EditTaskResult buildResponse(Task task) {
        return new EditTaskResult(
                task.getId(),
                task.getTitle(),
                task.getDeadlineDate(),
                task.getDeadlineTime(),
                task.getCategoryId(),
                task.getDifficultyId(),
                task.getUserId(),
                task.getCompletedAt(),
                task.getDescription()
        );
    }

}
