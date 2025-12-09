package pl.gamilife.grouptask.usecase.editgrouptask;

import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.task.TasksApi;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.grouptask.entity.GroupTask;
import pl.gamilife.grouptask.entity.GroupTaskMember;
import pl.gamilife.grouptask.exception.domain.GroupTaskNotFoundException;
import pl.gamilife.grouptask.repository.GroupTaskRepository;

import java.time.Instant;
import java.util.UUID;

@Service
public class EditGroupTaskUseCaseImpl implements EditGroupTaskUseCase {
    private final GroupTaskRepository groupTaskRepository;
    private final EditGroupTaskMapper editGroupTaskMapper;
    private final TasksApi tasksProvider;
    private final GroupApi groupsProvider;

    public EditGroupTaskUseCaseImpl(GroupTaskRepository groupTaskRepository, EditGroupTaskMapper editGroupTaskMapper, TasksApi tasksProvider, GroupApi groupApi) {
        this.groupTaskRepository = groupTaskRepository;
        this.editGroupTaskMapper = editGroupTaskMapper;
        this.tasksProvider = tasksProvider;
        this.groupsProvider = groupApi;
    }

    @Override
    @Transactional
    public EditGroupTaskResponse execute(UUID groupTaskId, EditGroupTaskRequest req) {
        GroupTask groupTask = groupTaskRepository.findByGroupTaskId(groupTaskId).orElseThrow(
                () -> new GroupTaskNotFoundException("Group Task with id:" + groupTaskId + " does not exist"));

        Boolean isAccepted = req.isAccepted();
        Instant acceptedDate = null;
        Instant completedAt = null;
        boolean changedToAccepted = isAccepted!=null&&isAccepted&&groupTask.getIsAccepted()==null;

        if (changedToAccepted) {
            acceptedDate = Instant.now();
            completedAt = Instant.now();
            for (GroupTaskMember taskMember : groupTask.getGroupTaskMembers()) {
                if (taskMember.getIsMarkedDone()!=null&& taskMember.getIsMarkedDone()) {
                    groupsProvider.editMemberWallet(taskMember.getGroupMemberId(), groupTask.getGroupId(), req.reward());
                }
            }
        }

        groupTask.setReward(req.reward());
        groupTask.setIsAccepted(req.isAccepted());
        groupTask.setAcceptedDate(acceptedDate);
        groupTask.setDeclineMessage(req.declineMessage());

        TaskForGroupTaskRequestDto taskRequestDto = new TaskForGroupTaskRequestDto(
                req.title(),
                req.deadline(),
                req.categoryId(),
                req.difficultyId(),
                completedAt != null,
                req.description()
        );

        UUID taskId=groupTask.getTaskId();
        tasksProvider.updateTaskForGroupTask(taskRequestDto, taskId);

        return editGroupTaskMapper.toResponse(groupTaskRepository.save(groupTask));
    }

}
