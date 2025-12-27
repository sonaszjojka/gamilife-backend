package pl.gamilife.grouptask.usecase.creategrouptask;

import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.task.TaskApi;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.grouptask.domain.context.GroupContext;
import pl.gamilife.grouptask.entity.GroupTask;
import pl.gamilife.grouptask.repository.GroupTaskRepository;

import java.util.UUID;

@Service
@AllArgsConstructor
public class CreateGroupTaskUseCaseImpl implements CreateGroupTaskUseCase {
    private final GroupContext groupContext;
    private final GroupTaskRepository groupTaskRepository;
    private final CreateGroupTaskMapper createGroupTaskMapper;
    private final TaskApi tasksProvider;

    @Override
    @Transactional
    public CreateGroupTaskResponse execute(CreateGroupTaskRequest request, UUID groupId) {

        TaskForGroupTaskRequestDto taskForGroupTaskRequestDto = new TaskForGroupTaskRequestDto(
                request.title(),
                request.deadlineDate(),
                null,
                request.deadlineTime(),
                groupContext.getCurrentGroupTimezone(groupId),
                request.categoryId(),
                request.difficultyId(),
                null,
                null,
                request.description()
        );


        GroupTask groupTask = createGroupTaskMapper.toEntity(request, groupId,
                tasksProvider.createTaskForGroupTask(taskForGroupTaskRequestDto).taskId());
        GroupTask savedGroupTask = groupTaskRepository.save(groupTask);


        return createGroupTaskMapper.toResponse(savedGroupTask);

    }
}
