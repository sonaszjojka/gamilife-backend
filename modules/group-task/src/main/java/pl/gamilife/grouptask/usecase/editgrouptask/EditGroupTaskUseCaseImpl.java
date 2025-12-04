package pl.gamilife.grouptask.usecase.editgrouptask;

import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;
import pl.gamilife.api.task.TasksApi;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.grouptask.entity.GroupTask;
import pl.gamilife.grouptask.exception.domain.GroupTaskNotFoundException;
import pl.gamilife.grouptask.repository.GroupTaskRepository;

import java.time.Instant;
import java.time.LocalDateTime;
import java.util.UUID;

@Service
public class EditGroupTaskUseCaseImpl implements EditGroupTaskUseCase {
    private final GroupTaskRepository groupTaskRepository;
    private final EditGroupTaskMapper editGroupTaskMapper;
    private final TasksApi tasksProvider;

    public EditGroupTaskUseCaseImpl(GroupTaskRepository groupTaskRepository, EditGroupTaskMapper editGroupTaskMapper, TasksApi tasksProvider) {
        this.groupTaskRepository = groupTaskRepository;
        this.editGroupTaskMapper = editGroupTaskMapper;
        this.tasksProvider = tasksProvider;
    }

    @Override
    @Transactional
    public EditGroupTaskResponse execute(UUID groupTaskId, EditGroupTaskRequest req) {

        GroupTask groupTask = groupTaskRepository.findByGroupTaskId(groupTaskId).orElseThrow(
                () -> new GroupTaskNotFoundException("Group Task with id:" + groupTaskId + " does not exist"));




        Boolean isAccepted = req.isAccepted();
        Instant acceptedDate = null;
        LocalDateTime completedAt = null;
        boolean changedToAccepted = Boolean.TRUE.equals(isAccepted)
                && !Boolean.TRUE.equals(groupTask.getIsAccepted());

        if (changedToAccepted) {
            acceptedDate = Instant.now();
            completedAt=LocalDateTime.now();
        }

        groupTask.setReward(req.reward());
        groupTask.setIsAccepted(req.isAccepted());
        groupTask.setAcceptedDate(acceptedDate);
        groupTask.setDeclineMessage(req.declineMessage());


        TaskForGroupTaskRequestDto taskRequestDto = new TaskForGroupTaskRequestDto(
                req.title(),
                req.startTime(),
                req.endTime(),
                req.categoryId(),
                req.difficultyId(),
                completedAt,
                req.description()
        );

        UUID taskId=groupTask.getTaskId();
        tasksProvider.updateTaskForGroupTask(taskRequestDto, taskId);
        return editGroupTaskMapper.toResponse(groupTaskRepository.save(groupTask));
    }


}
