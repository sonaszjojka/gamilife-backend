package edu.pjwstk.groups.usecase.creategrouprequest;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.groups.enums.GroupRequestStatusEnum;
import edu.pjwstk.groups.enums.GroupTypeEnum;
import edu.pjwstk.groups.exception.domain.GroupFullException;
import edu.pjwstk.groups.exception.domain.GroupRequestStatusNotFoundException;
import edu.pjwstk.groups.exception.domain.InvalidGroupDataException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupRequest;
import edu.pjwstk.groups.model.GroupRequestStatus;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import edu.pjwstk.groups.repository.GroupRequestJpaRepository;
import edu.pjwstk.groups.repository.GroupRequestStatusJpaRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@AllArgsConstructor
public class CreateGroupRequestUseCaseImpl implements CreateGroupRequestUseCase {

    private final GroupRequestJpaRepository groupRequestRepository;
    private final GroupJpaRepository groupRepository;
    private final GroupMemberJpaRepository groupMemberRepository;
    private final GroupRequestStatusJpaRepository groupRequestStatusRepository;
    private final AuthApi authApi;

    @Override
    @Transactional
    public CreateGroupRequestResult executeInternal(CreateGroupRequestCommand cmd) {
        Group group = getGroupWithMembers(cmd.groupId());
        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (isUserMemberOfGroup(currentUserDto.userId(), group)) {
            throw new InvalidGroupDataException("User with id: " + currentUserDto.userId()
                    + " is already added to group with id:" + cmd.groupId());
        }

        if (!group.isOfType(GroupTypeEnum.REQUEST_ONLY)) {
            throw new InvalidGroupDataException("This group does not accept join requests. " +
                    "Only 'REQUEST_ONLY' groups can receive join requests.");
        }

        GroupRequestStatus sentGroupRequestStatus = getSentGroupRequestStatus();
        if (hasUserSentRequestToGroup(group, currentUserDto, sentGroupRequestStatus)) {
            throw new InvalidGroupDataException("User with id: " + currentUserDto.userId()
                    + " has already group request with status: SENT to group with id:" + cmd.groupId());
        }

        if (group.isFull()) {
            throw new GroupFullException("Group is full – member limit reached: " + group.getMembersLimit());
        }

        GroupRequest groupRequest = createGroupRequest(
                currentUserDto.userId(),
                group,
                sentGroupRequestStatus
        );

        return createGroupRequestResult(groupRequest);
    }

    private boolean hasUserSentRequestToGroup(Group group, CurrentUserDto currentUserDto, GroupRequestStatus sentGroupRequestStatus) {
        return groupRequestRepository.existsByGroupRequestedAndUserIdAndGroupRequestStatus(group, currentUserDto.userId(), sentGroupRequestStatus);
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
        return groupRepository.findWithGroupMembersByGroupId(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));
    }

    private GroupRequest createGroupRequest(UUID userId, Group group, GroupRequestStatus groupRequestStatus) {
        GroupRequest groupRequest = GroupRequest.builder()
                .groupRequestId(UUID.randomUUID())
                .userId(userId)
                .groupRequested(group)
                .groupRequestStatus(groupRequestStatus)
                .build();

        return groupRequestRepository.save(groupRequest);
    }

    private CreateGroupRequestResult createGroupRequestResult(GroupRequest groupRequest) {
        return CreateGroupRequestResult.builder()
                .groupRequestId(groupRequest.getGroupRequestId())
                .userId(groupRequest.getUserId())
                .groupRequested(new CreateGroupRequestResult.GroupDto(groupRequest.getGroupId()))
                .createdAt(groupRequest.getCreatedAt())
                .groupRequestStatus(new CreateGroupRequestResult.GroupRequestStatusDto(
                        groupRequest.getGroupRequestStatus().getGroupRequestStatusId(),
                        groupRequest.getGroupRequestStatus().getTitle()
                ))
                .build();
    }
}
