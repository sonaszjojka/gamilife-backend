package edu.pjwstk.grouptasks.usecase.creategrouptask;
import edu.pjwstk.common.tasksApi.TasksApi;
import edu.pjwstk.common.tasksApi.exception.TaskNotFoundException;
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
        //Todo: Create task on create group task request
        /*
        if (!tasksProvider.taskExistsByTaskId(request.taskId()))
        {
            throw new TaskNotFoundException("Task with id:" + request.taskId() + " does not exist");
        }
        */
        GroupTask groupTask = createGroupTaskMapper.toEntity(request,UUID.randomUUID(),groupId,UUID.randomUUID());
        GroupTask savedGroupTask = groupTaskRepository.save(groupTask);
        return createGroupTaskMapper.toResponse(savedGroupTask);

    }
}
