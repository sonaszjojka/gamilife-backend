package pl.gamilife.grouptask.usecase.creategrouptaskmember;

import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.group.dto.GroupMemberDto;
import pl.gamilife.api.task.TaskApi;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.grouptask.entity.GroupTask;
import pl.gamilife.grouptask.entity.GroupTaskMember;
import pl.gamilife.grouptask.exception.domain.GroupTaskNotFoundException;
import pl.gamilife.grouptask.repository.GroupTaskMemberRepository;
import pl.gamilife.grouptask.repository.GroupTaskRepository;
import pl.gamilife.shared.kernel.event.GroupTaskMemberAdded;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

import java.util.UUID;

@Service
@AllArgsConstructor
public class CreateGroupTaskMemberUseCaseImpl implements CreateGroupTaskMemberUseCase {

    private final GroupTaskMemberRepository groupTaskMemberRepository;
    private final CreateGroupTaskMemberMapper createGroupTaskMemberMapper;
    private final GroupApi groupApi;
    private final TaskApi taskApi;
    private final GroupTaskRepository groupTaskRepository;
    private final ApplicationEventPublisher eventPublisher;

    @Override
    public CreateGroupTaskMemberResponse execute(UUID groupTaskId, UUID groupId, CreateGroupTaskMemberRequest request) {
        GroupMemberDto groupMember = groupApi.findGroupMemberById(groupId, request.groupMemberId());
        GroupTask groupTask = groupTaskRepository.findByGroupTaskId(groupTaskId).orElseThrow(
                () -> new GroupTaskNotFoundException("Group task with id:" + groupTaskId + " does not exist"));

        TaskDto task = taskApi.findTaskById(groupTask.getTaskId());

        if (!groupTask.belongsToGroup(groupId)) {
            throw new ResourceOwnerPrivilegesRequiredException("Group task with id:" + groupTaskId + " does not belong to group with id:" + groupTask.getGroupId());
        }

        eventPublisher.publishEvent(new GroupTaskMemberAdded(
                groupMember.userId(),
                groupTaskId,
                task.title(),
                groupId,
                groupMember.memberGroup().groupName()
        ));

        GroupTaskMember groupTaskMember = createGroupTaskMemberMapper.toEntity(groupTask, request.groupMemberId());
        GroupTaskMember savedGroupTaskMember = groupTaskMemberRepository.save(groupTaskMember);
        return createGroupTaskMemberMapper.toResponse(savedGroupTaskMember);
    }
}
