package edu.pjwstk.grouptasks.usecase.creategrouptask;
import edu.pjwstk.common.tasksApi.TasksApi;
import edu.pjwstk.grouptasks.entity.GroupTask;
import edu.pjwstk.grouptasks.repository.GroupTaskRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Component;

import java.time.Instant;
import java.util.UUID;

@Component
public class CreateGroupTaskUseCaseImpl implements CreateGroupTaskUseCase {
    private final GroupTaskRepository groupTaskRepository;
    private final CreateGroupTaskMapper createGroupTaskMapper;
    private final TasksApi tasksProvider;

    public CreateGroupTaskUseCaseImpl(GroupTaskRepository groupTaskRepository, CreateGroupTaskMapper createGroupTaskMapper, TasksApi tasksProvider) {
        this.groupTaskRepository = groupTaskRepository;
        this.createGroupTaskMapper = createGroupTaskMapper;
        this.tasksProvider = tasksProvider;
    }

    @Override
    @Transactional
    public CreateGroupTaskResponse execute(CreateGroupTaskRequest request, UUID taskId) {
        if (!tasksProvider.taskExistsByTaskId(taskId))
        {
            throw new IllegalArgumentException("Task with id:" + taskId + " does not exist");
        }
        GroupTask groupTask = createGroupTaskMapper.toEntity(request,UUID.randomUUID(),taskId);
        GroupTask savedGroupTask = groupTaskRepository.save(groupTask);
        return createGroupTaskMapper.toResponse(savedGroupTask);

    }
}
