package edu.pjwstk.groups.usecase.editgroupinvitationstatus;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.core.exception.common.domain.ResourceOwnerPrivilegesRequiredException;
import edu.pjwstk.groups.enums.InvitationStatusEnum;
import edu.pjwstk.groups.exception.domain.*;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupInvitation;
import edu.pjwstk.groups.model.GroupMember;
import edu.pjwstk.groups.model.InvitationStatus;
import edu.pjwstk.groups.repository.GroupInvitationJpaRepository;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.InvitationStatusJpaRepository;
import edu.pjwstk.groups.service.GroupMemberService;
import edu.pjwstk.groups.util.GroupInvitationUtil;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@AllArgsConstructor
public class EditGroupInvitationStatusUseCaseImpl implements EditGroupInvitationStatusUseCase {

    private final GroupInvitationJpaRepository groupInvitationRepository;
    private final InvitationStatusJpaRepository invitationStatusRepository;
    private final AuthApi authApi;
    private final GroupMemberService groupMemberService;
    private final GroupInvitationUtil groupInvitationUtil;
    private final GroupJpaRepository groupRepository;

    @Override
    @Transactional
    public EditGroupInvitationStatusResult executeInternal(EditGroupInvitationStatusCommand cmd) {
        Group group = getGroupWithMembers(cmd.groupId());
        GroupInvitation groupInvitation = getGroupInvitationWithGroup(cmd.groupInvitationId(), cmd.groupId());
        InvitationStatus newInvitationStatus = getInvitationStatus(cmd.invitationStatusId());
        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (groupInvitation.doesBelongToUser(currentUserDto.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException(
                    "Only user who is assigned to this invitation can change group invitation status!");
        }

        if (!groupInvitationUtil.verifyToken(cmd.token(), groupInvitation.getTokenHash())) {
            throw new InvalidGroupInvitationTokenException("Invalid or tampered group invitation token!");
        }

        if (groupInvitation.isExpired()) {
            throw new GroupInvitationExpiredException("Group invitation has expired at: "
                    + groupInvitation.getExpiresAt());
        }

        if (!groupInvitation.hasStatus(InvitationStatusEnum.SENT)) {
            throw new InvalidGroupInvitationDataException(
                    "Group invitation with status ACCEPTED or DECLINED are final and cannot be changed!");
        }

        if (newInvitationStatus.toEnum() == InvitationStatusEnum.SENT) {
            throw new InvalidGroupDataException(
                    "Group invitation with id: " + cmd.groupInvitationId() + " has already status: SENT");
        }

        // If invitation is accepted, create a new group member
        GroupMember groupMember = null;
        if (newInvitationStatus.toEnum() == InvitationStatusEnum.ACCEPTED) {
            groupMember = groupMemberService.createGroupMember(
                    group,
                    groupInvitation.getUserId()
            );
        }

        groupInvitation.setInvitationStatus(newInvitationStatus);
        GroupInvitation savedGroupInvitation = groupInvitationRepository.save(groupInvitation);

        return buildEditGroupInvitationStatusResponse(
                savedGroupInvitation,
                groupMember != null ? groupMember.getGroupMemberId() : null
        );
    }

    private Group getGroupWithMembers(UUID groupId) {
        return groupRepository.findWithGroupMembersByGroupId(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));
    }

    private InvitationStatus getInvitationStatus(Integer invitationStatusId) {
        return invitationStatusRepository.findById(invitationStatusId)
                .orElseThrow(() -> new InvitationStatusNotFoundException("Group invitation status with id: " +
                        invitationStatusId + " not found"));
    }

    private GroupInvitation getGroupInvitationWithGroup(UUID groupInvitationId, UUID groupId) {
        return groupInvitationRepository.findWithGroupByGroupInvitationIdAndGroupId(groupInvitationId, groupId)
                .orElseThrow(() -> new GroupInvitationNotFoundException("Group invitation with id: " + groupInvitationId
                        + " not found!"));
    }

    private EditGroupInvitationStatusResult buildEditGroupInvitationStatusResponse(
            GroupInvitation groupInvitation,
            UUID groupMemberId
    ) {
        return EditGroupInvitationStatusResult.builder()
                .groupInvitationId(groupInvitation.getGroupInvitationId())
                .groupInvited(new EditGroupInvitationStatusResult.GroupDto(
                        groupInvitation.getGroupId()
                ))
                .userId(groupInvitation.getUserId())
                .expiresAt(groupInvitation.getExpiresAt())
                .mailSentAt(groupInvitation.getMailSentAt())
                .link(groupInvitation.getLink())
                .invitationStatus(new EditGroupInvitationStatusResult.InvitationStatusDto(
                        groupInvitation.getInvitationStatus().getInvitationStatusId(),
                        groupInvitation.getInvitationStatus().getTitle()
                ))
                .groupMemberId(groupMemberId)
                .build();
    }
}
