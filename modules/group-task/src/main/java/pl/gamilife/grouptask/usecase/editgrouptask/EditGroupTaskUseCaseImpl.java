package pl.gamilife.grouptask.usecase.editgrouptask;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.task.TaskApi;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.grouptask.domain.context.GroupContext;
import pl.gamilife.grouptask.entity.GroupTask;
import pl.gamilife.grouptask.entity.GroupTaskMember;
import pl.gamilife.grouptask.exception.domain.GroupTaskNotFoundException;
import pl.gamilife.grouptask.repository.GroupTaskRepository;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class EditGroupTaskUseCaseImpl implements EditGroupTaskUseCase {
    private final GroupTaskRepository groupTaskRepository;
    private final EditGroupTaskMapper editGroupTaskMapper;
    private final TaskApi tasksProvider;
    private final GroupApi groupsProvider;
    private final GroupContext groupContext;

    @Override
    public EditGroupTaskResponse execute(UUID groupTaskId, EditGroupTaskRequest req) {
        GroupTask groupTask = groupTaskRepository.findByGroupTaskId(groupTaskId).orElseThrow(
                () -> new GroupTaskNotFoundException("Group Task with id:" + groupTaskId + " does not exist"));

        boolean changedToAccepted = Boolean.TRUE.equals(req.isAccepted()) && !groupTask.isAccepted();
        boolean changedToNotAccepted = Boolean.FALSE.equals(req.isAccepted()) && groupTask.isAccepted();
        if (changedToAccepted) {
            groupTask.accept();

            if (groupTask.isRewarded() && !groupTask.wasRewardIssued()) {
                for (GroupTaskMember taskMember : groupTask.getGroupTaskMembers()) {
                    if (taskMember.getMarkedDoneAt() != null) {
                        groupsProvider.editMemberWallet(taskMember.getGroupMemberId(), groupTask.getGroupId(), req.reward());
                    }
                }

                groupTask.markRewardsAsIssued();
            }
        } else if (changedToNotAccepted) {
            groupTask.undoAcceptation();
        }

        groupTask.setReward(req.reward());
        groupTask.setDeclineMessage(req.declineMessage());

        TaskForGroupTaskRequestDto taskRequestDto = new TaskForGroupTaskRequestDto(
                req.title(),
                req.deadlineDate(),
                req.removeDeadlineTime(),
                req.deadlineTime(),
                groupContext.getCurrentGroupTimezone(groupTask.getGroupId()),
                req.categoryId(),
                req.difficultyId(),
                req.isAccepted(),
                req.removeDescription(),
                req.description()
        );

        UUID taskId = groupTask.getTaskId();
        tasksProvider.updateTaskForGroupTask(taskRequestDto, taskId);

        return editGroupTaskMapper.toResponse(groupTaskRepository.save(groupTask));
    }

}
