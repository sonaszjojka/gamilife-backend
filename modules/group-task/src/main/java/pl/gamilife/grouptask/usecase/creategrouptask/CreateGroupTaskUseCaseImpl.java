package pl.gamilife.grouptask.usecase.creategrouptask;

import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;
import pl.gamilife.api.task.TasksApi;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.grouptask.entity.GroupTask;
import pl.gamilife.grouptask.repository.GroupTaskRepository;

import java.util.UUID;

@Service
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
    public CreateGroupTaskResponse execute(CreateGroupTaskRequest request, UUID groupId) {

        TaskForGroupTaskRequestDto taskForGroupTaskRequestDto = new TaskForGroupTaskRequestDto(
                request.title(),
                request.deadline(),
                request.categoryId(),
                request.difficultyId(),
                null,
                request.description()
        );


        GroupTask groupTask = createGroupTaskMapper.toEntity(request, UUID.randomUUID(), groupId,
                tasksProvider.createTaskForGroupTask(taskForGroupTaskRequestDto).taskId());
        GroupTask savedGroupTask = groupTaskRepository.save(groupTask);


        return createGroupTaskMapper.toResponse(savedGroupTask);

    }
}
