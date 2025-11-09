package edu.pjwstk.groups.usecase.editgroupinvitationstatus;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.ResourceOwnerPrivilegesRequiredException;
import edu.pjwstk.groups.exception.domain.*;
import edu.pjwstk.groups.model.GroupInvitation;
import edu.pjwstk.groups.model.GroupMember;
import edu.pjwstk.groups.model.InvitationStatus;
import edu.pjwstk.groups.repository.GroupInvitationJpaRepository;
import edu.pjwstk.groups.repository.InvitationStatusJpaRepository;
import edu.pjwstk.groups.service.GroupMemberService;
import edu.pjwstk.groups.enums.InvitationStatusEnum;
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

    @Override
    @Transactional
    public EditGroupInvitationStatusResult executeInternal(EditGroupInvitationStatusCommand cmd) {
        GroupInvitation groupInvitation = getGroupInvitation(cmd.groupInvitationId(), cmd.groupId());
        InvitationStatus newInvitationStatus = getInvitationStatus(cmd.invitationStatusId());
        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (groupInvitation.doesBelongToGroup(cmd.groupId())) {
            throw new ResourceOwnerPrivilegesRequiredException(
                    "Invitation " + cmd.groupInvitationId() + " does not belong to group " + cmd.groupId() + "!");
        }

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
                    groupInvitation.getGroupInvited(),
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

    private InvitationStatus getInvitationStatus(Integer invitationStatusId) {
        return invitationStatusRepository.findById(invitationStatusId)
                .orElseThrow(() -> new InvitationStatusNotFoundException("Group invitation status with id: " +
                        invitationStatusId + " not found"));
    }

    private GroupInvitation getGroupInvitation(UUID groupInvitationId, UUID groupId) {
        return groupInvitationRepository.findByGroupInvitationIdAndGroupInvited_GroupId(groupInvitationId, groupId)
                .orElseThrow(() -> new GroupInvitationNotFoundException("Group invitation with id: " + groupInvitationId
                        + " not found!"));
    }

    private EditGroupInvitationStatusResult buildEditGroupInvitationStatusResponse(
            GroupInvitation groupInvitation,
            UUID groupMemberId
    ) {
        return EditGroupInvitationStatusResult.builder()
                .groupInvitationId(groupInvitation.getGroupInvitationId())
                .groupInvited(
                        groupInvitation.getGroupInvited() != null
                                ? EditGroupInvitationStatusResult.GroupDto.builder()
                                .groupId(groupInvitation.getGroupInvited().getGroupId())
                                .build()
                                : null
                )
                .userId(groupInvitation.getUserId())
                .expiresAt(groupInvitation.getExpiresAt())
                .mailSentAt(groupInvitation.getMailSentAt())
                .link(groupInvitation.getLink())
                .invitationStatus(
                        groupInvitation.getInvitationStatus() != null
                                ? EditGroupInvitationStatusResult.InvitationStatusDto.builder()
                                .invitationStatusId(groupInvitation.getInvitationStatus().getInvitationStatusId())
                                .title(groupInvitation.getInvitationStatus().getTitle())
                                .build()
                                : null
                )
                .groupMemberId(groupMemberId)
                .build();
    }
}
