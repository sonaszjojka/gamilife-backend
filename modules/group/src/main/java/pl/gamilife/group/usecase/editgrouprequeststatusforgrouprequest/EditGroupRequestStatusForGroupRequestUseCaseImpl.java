package pl.gamilife.group.usecase.editgrouprequeststatusforgrouprequest;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.group.enums.GroupRequestStatusEnum;
import pl.gamilife.group.exception.domain.GroupRequestNotFoundException;
import pl.gamilife.group.exception.domain.GroupRequestStatusNotFoundException;
import pl.gamilife.group.exception.domain.InvalidGroupDataException;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.model.GroupRequest;
import pl.gamilife.group.model.GroupRequestStatus;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.group.repository.GroupRequestJpaRepository;
import pl.gamilife.group.repository.GroupRequestStatusJpaRepository;
import pl.gamilife.group.service.GroupMemberService;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.GroupNotFoundException;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class EditGroupRequestStatusForGroupRequestUseCaseImpl implements EditGroupRequestStatusForGroupRequestUseCase {

    private final GroupRequestJpaRepository groupRequestRepository;
    private final GroupRequestStatusJpaRepository groupRequestStatusRepository;
    private final AuthApi authApi;
    private final GroupMemberService groupMemberService;
    private final GroupJpaRepository groupJpaRepository;

    @Override
    public EditGroupRequestStatusForGroupRequestResult execute(EditGroupRequestStatusForGroupRequestCommand cmd) {
        Group group = getGroupWithMembers(cmd.groupId());
        GroupRequest groupRequest = getGroupRequest(cmd.groupId(), cmd.groupRequestId());
        GroupRequestStatus newGroupRequestStatus = getGroupRequestStatus(cmd.newGroupRequestStatusId());
        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (!group.isUserAdmin(currentUserDto.userId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can change status group request!");
        }

        if (!groupRequest.hasStatus(GroupRequestStatusEnum.SENT)) {
            throw new InvalidGroupDataException("Group requests with status ACCEPTED or DECLINED are final and cannot be changed!");
        }

        if (newGroupRequestStatus.toEnum() == GroupRequestStatusEnum.SENT) {
            throw new InvalidGroupDataException("Group requests with id: " + cmd.groupRequestId() + " has already status: SENT");
        }

        // If the request is accepted, create a new group member
        GroupMember groupMember = null;
        if (newGroupRequestStatus.toEnum() == GroupRequestStatusEnum.ACCEPTED) {
            groupMember = groupMemberService.createGroupMember(
                    group,
                    groupRequest.getUserId()
            );
        }

        groupRequest.setGroupRequestStatus(newGroupRequestStatus);
        GroupRequest savedGroupRequest = groupRequestRepository.save(groupRequest);

        return buildEditGroupRequestStatusForGroupRequestResult(
                savedGroupRequest,
                groupMember != null ? groupMember.getGroupMemberId() : null
        );
    }

    private Group getGroupWithMembers(UUID groupId) {
        return groupJpaRepository.findWithGroupMembersByGroupId(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));
    }

    private GroupRequest getGroupRequest(UUID groupId, UUID groupRequestId) {
        return groupRequestRepository.findByGroupRequestIdAndGroupRequested_GroupId(groupRequestId, groupId)
                .orElseThrow(() -> new GroupRequestNotFoundException("Group request with id:" + groupRequestId + " not found!"));
    }

    private GroupRequestStatus getGroupRequestStatus(Integer groupRequestStatusId) {
        return groupRequestStatusRepository.findById(groupRequestStatusId)
                .orElseThrow(() -> new GroupRequestStatusNotFoundException("Group request status with id: "
                        + groupRequestStatusId + " not found!"));
    }

    private EditGroupRequestStatusForGroupRequestResult buildEditGroupRequestStatusForGroupRequestResult(
            GroupRequest groupRequest,
            UUID groupMemberId
    ) {
        return EditGroupRequestStatusForGroupRequestResult.builder()
                .groupRequestId(groupRequest.getGroupRequestId())
                .userId(groupRequest.getUserId())
                .groupRequested(new EditGroupRequestStatusForGroupRequestResult.GroupDto(groupRequest.getGroupId()))
                .createdAt(groupRequest.getCreatedAt())
                .groupRequestStatus(new EditGroupRequestStatusForGroupRequestResult.GroupRequestStatusDto(
                        groupRequest.getGroupRequestStatus().getGroupRequestStatusId(),
                        groupRequest.getGroupRequestStatus().getTitle()
                ))
                .groupMemberId(groupMemberId)
                .build();
    }
}
