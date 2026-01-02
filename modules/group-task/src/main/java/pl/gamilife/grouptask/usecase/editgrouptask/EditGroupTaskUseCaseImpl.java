package pl.gamilife.grouptask.usecase.editgrouptask;

import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.group.dto.BasicGroupMemberDto;
import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.api.task.TaskApi;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.grouptask.domain.context.GroupContext;
import pl.gamilife.grouptask.entity.GroupTask;
import pl.gamilife.grouptask.entity.GroupTaskMember;
import pl.gamilife.grouptask.exception.domain.GroupTaskNotFoundException;
import pl.gamilife.grouptask.repository.GroupTaskRepository;
import pl.gamilife.shared.kernel.event.GroupTaskCompletedEvent;
import pl.gamilife.shared.kernel.event.GroupTaskUndoneEvent;

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
    private final ApplicationEventPublisher eventPublisher;

    @Override
    public EditGroupTaskResponse execute(UUID groupTaskId, EditGroupTaskRequest req) {
        GroupTask groupTask = groupTaskRepository.findWithMembersByGroupTaskId(groupTaskId).orElseThrow(
                () -> new GroupTaskNotFoundException("Group Task with id:" + groupTaskId + " does not exist"));

        boolean changedToAccepted = Boolean.TRUE.equals(req.isAccepted()) && !groupTask.isAccepted();
        boolean changedToNotAccepted = Boolean.FALSE.equals(req.isAccepted()) && groupTask.isAccepted();
        if (changedToAccepted) {
            groupTask.accept();

            TaskDto task = tasksProvider.findTaskById(groupTask.getTaskId());
            GroupDto group = groupsProvider.findGroupById(groupTask.getGroupId());

            var doneGroupTaskMembers = groupTask.getGroupTaskMembers()
                    .stream()
                    .filter(GroupTaskMember::isMarkedDone)
                    .toList();

            var doneGroupTaskMemberIds = doneGroupTaskMembers.stream()
                    .map(GroupTaskMember::getGroupMemberId)
                    .toList();

            if (groupTask.isRewarded() && !groupTask.wasRewardIssued()) {
                doneGroupTaskMembers.forEach(GroupTaskMember::markRewardAsIssued);
                var doneMemberDetails = groupsProvider.grantRewards(doneGroupTaskMemberIds, req.reward());
                var doneMemberUserIds = doneMemberDetails.stream()
                        .map(BasicGroupMemberDto::userId)
                        .toList();

                eventPublisher.publishEvent(new GroupTaskCompletedEvent(
                        groupTask.getGroupId(),
                        group.groupName(),
                        groupTask.getId(),
                        task.title(),
                        doneMemberUserIds,
                        groupTask.wasRewardIssued()
                ));
                groupTask.markRewardsAsIssued();
            } else {
                var memberIds = groupContext.findMembersByIdIn(doneGroupTaskMemberIds)
                        .stream()
                        .map(BasicGroupMemberDto::userId)
                        .toList();
                eventPublisher.publishEvent(new GroupTaskCompletedEvent(
                        groupTask.getGroupId(),
                        group.groupName(),
                        groupTask.getId(),
                        task.title(),
                        memberIds,
                        groupTask.wasRewardIssued()
                ));
            }
        } else if (changedToNotAccepted) {
            var previouslyDoneGroupTaskMembers = groupTask.getGroupTaskMembers()
                    .stream()
                    .filter(GroupTaskMember::isMarkedDone)
                    .map(GroupTaskMember::getGroupMemberId)
                    .toList();
            var memberIds = groupContext.findMembersByIdIn(previouslyDoneGroupTaskMembers)
                    .stream()
                    .map(BasicGroupMemberDto::userId)
                    .toList();

            eventPublisher.publishEvent(new GroupTaskUndoneEvent(memberIds));

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
