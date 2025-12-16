package pl.gamilife.task.application.getusertasks;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.task.domain.model.filter.TaskFilter;
import pl.gamilife.task.domain.port.context.UserContext;
import pl.gamilife.task.domain.port.repository.TaskRepository;

import java.time.LocalDateTime;
import java.time.ZoneId;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class GetUserTasksUseCaseImpl implements GetUserTasksUseCase {

    private final TaskRepository taskRepository;
    private final UserContext userContext;

    @Override
    public Page<GetUserTasksResult> execute(GetUserTasksCommand cmd) {
        TaskFilter filter = new TaskFilter(
                cmd.userId(),
                cmd.categoryId(),
                cmd.difficultyId(),
                cmd.isGroupTask(),
                cmd.isCompleted()
        );

        ZoneId zoneId = cmd.zoneId() == null ? userContext.getCurrentUserTimezone(cmd.userId()) : cmd.zoneId();

        return taskRepository.findAll(filter, cmd.pageNumber(), cmd.pageSize())
                .map(task -> new GetUserTasksResult(
                        task.getId(),
                        GetUserTasksResult.TaskType.TASK,
                        task.getTitle(),
                        task.getDescription(),
                        task.getDeadlineDate(),
                        task.getDeadlineTime(),
                        task.getCategory().getId(),
                        task.getDifficulty().getId(),
                        switch (task.calculateCurrentStatus(LocalDateTime.now(zoneId))) {
                            case INCOMPLETE -> GetUserTasksResult.TaskStatus.INCOMPLETE;
                            case COMPLETED -> GetUserTasksResult.TaskStatus.COMPLETED;
                            case DEADLINE_MISSED -> GetUserTasksResult.TaskStatus.DEADLINE_MISSED;
                            case DEADLINE_TODAY -> GetUserTasksResult.TaskStatus.DEADLINE_TODAY;
                        },
                        task.getCategory().getName(),
                        task.getDifficulty().getName(),
                        task.isGroupTask()
                ));
    }
}
