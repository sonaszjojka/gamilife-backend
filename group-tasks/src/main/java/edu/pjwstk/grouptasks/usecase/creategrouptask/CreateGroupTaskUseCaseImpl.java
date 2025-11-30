package edu.pjwstk.grouptasks.usecase.creategrouptask;

import edu.pjwstk.api.tasks.TasksApi;
import edu.pjwstk.api.tasks.dto.TaskForGroupTaskRequestDto;
import edu.pjwstk.grouptasks.entity.GroupTask;
import edu.pjwstk.grouptasks.repository.GroupTaskRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

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
                request.startTime(),
                request.endTime(),
                request.categoryId(),
                request.difficultyId(),
                request.completedAt(),
                request.description()
        );


        GroupTask groupTask = createGroupTaskMapper.toEntity(request,UUID.randomUUID(),groupId,
                                                            tasksProvider.createTaskForGroupTask(taskForGroupTaskRequestDto).taskId());
        GroupTask savedGroupTask = groupTaskRepository.save(groupTask);


        return createGroupTaskMapper.toResponse(savedGroupTask);

    }
}
