package edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.GroupAdminPrivilegesRequiredException;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.groups.enums.GroupRequestStatusEnum;
import edu.pjwstk.groups.exception.domain.GroupRequestNotFoundException;
import edu.pjwstk.groups.exception.domain.GroupRequestStatusNotFoundException;
import edu.pjwstk.groups.exception.domain.InvalidGroupDataException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;
import edu.pjwstk.groups.model.GroupRequest;
import edu.pjwstk.groups.model.GroupRequestStatus;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.GroupRequestJpaRepository;
import edu.pjwstk.groups.repository.GroupRequestStatusJpaRepository;
import edu.pjwstk.groups.service.GroupMemberService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@AllArgsConstructor
public class EditGroupRequestStatusForGroupRequestUseCaseImpl implements EditGroupRequestStatusForGroupRequestUseCase {

    private final GroupRequestJpaRepository groupRequestRepository;
    private final GroupRequestStatusJpaRepository groupRequestStatusRepository;
    private final AuthApi authApi;
    private final GroupMemberService groupMemberService;
    private final GroupJpaRepository groupJpaRepository;

    @Override
    @Transactional
    public EditGroupRequestStatusForGroupRequestResult executeInternal(EditGroupRequestStatusForGroupRequestCommand cmd) {
        Group group = getGroupWithMembers(cmd.groupId());
        GroupRequest groupRequest = getGroupRequest(cmd.groupId(), cmd.groupRequestId());
        GroupRequestStatus newGroupRequestStatus = getGroupRequestStatus(cmd.newGroupRequestStatusId());
        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (group.isUserAdmin(currentUserDto.userId())) {
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
