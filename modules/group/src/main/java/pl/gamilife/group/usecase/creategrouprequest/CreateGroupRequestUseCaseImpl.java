package pl.gamilife.group.usecase.creategrouprequest;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.group.enums.GroupRequestStatusEnum;
import pl.gamilife.group.enums.GroupTypeEnum;
import pl.gamilife.group.exception.domain.GroupFullException;
import pl.gamilife.group.exception.domain.GroupRequestStatusNotFoundException;
import pl.gamilife.group.exception.domain.InvalidGroupDataException;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupRequest;
import pl.gamilife.group.model.GroupRequestStatus;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.group.repository.GroupMemberJpaRepository;
import pl.gamilife.group.repository.GroupRequestJpaRepository;
import pl.gamilife.group.repository.GroupRequestStatusJpaRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupNotFoundException;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class CreateGroupRequestUseCaseImpl implements CreateGroupRequestUseCase {

    private final GroupRequestJpaRepository groupRequestRepository;
    private final GroupJpaRepository groupRepository;
    private final GroupMemberJpaRepository groupMemberRepository;
    private final GroupRequestStatusJpaRepository groupRequestStatusRepository;

    @Override
    public CreateGroupRequestResult execute(CreateGroupRequestCommand cmd) {
        Group group = getGroupWithMembers(cmd.groupId());

        if (isUserMemberOfGroup(cmd.userId(), group)) {
            throw new InvalidGroupDataException("User with id: " + cmd.userId()
                    + " is already added to group with id:" + cmd.groupId());
        }

        if (!group.isOfType(GroupTypeEnum.REQUEST_ONLY)) {
            throw new InvalidGroupDataException("This group does not accept join requests. " +
                    "Only 'REQUEST_ONLY' groups can receive join requests.");
        }

        GroupRequestStatus sentGroupRequestStatus = getSentGroupRequestStatus();
        if (hasUserSentRequestToGroup(group, cmd.userId(), sentGroupRequestStatus)) {
            throw new InvalidGroupDataException("User with id: " + cmd.userId()
                    + " has already group request with status: SENT to group with id:" + cmd.groupId());
        }

        if (group.isFull()) {
            throw new GroupFullException("Group is full – member limit reached: " + group.getMembersLimit());
        }

        GroupRequest groupRequest = createGroupRequest(
                cmd.userId(),
                group,
                sentGroupRequestStatus
        );

        return createGroupRequestResult(groupRequest);
    }

    private boolean hasUserSentRequestToGroup(Group group, UUID userId, GroupRequestStatus sentGroupRequestStatus) {
        return groupRequestRepository.existsByGroupAndUserIdAndStatus(group, userId, sentGroupRequestStatus);
    }

    private GroupRequestStatus getSentGroupRequestStatus() {
        return groupRequestStatusRepository.findById(GroupRequestStatusEnum.SENT.getId())
                .orElseThrow(() -> new GroupRequestStatusNotFoundException("Group request status with id: "
                        + GroupRequestStatusEnum.SENT.getId() + " not found!"));
    }

    private boolean isUserMemberOfGroup(UUID userId, Group group) {
        return groupMemberRepository.existsByUserIdAndGroupAndLeftAt(userId, group, null);
    }

    private Group getGroupWithMembers(UUID groupId) {
        return groupRepository.findWithActiveMembersById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));
    }

    private GroupRequest createGroupRequest(UUID userId, Group group, GroupRequestStatus groupRequestStatus) {
        GroupRequest groupRequest = GroupRequest.create(group, userId, groupRequestStatus);

        return groupRequestRepository.save(groupRequest);
    }

    private CreateGroupRequestResult createGroupRequestResult(GroupRequest groupRequest) {
        return CreateGroupRequestResult.builder()
                .groupRequestId(groupRequest.getId())
                .userId(groupRequest.getUserId())
                .groupRequested(new CreateGroupRequestResult.GroupDto(groupRequest.getGroupId()))
                .createdAt(groupRequest.getCreatedAt())
                .groupRequestStatus(new CreateGroupRequestResult.GroupRequestStatusDto(
                        groupRequest.getStatus().getId(),
                        groupRequest.getStatus().getTitle()
                ))
                .build();
    }
}
