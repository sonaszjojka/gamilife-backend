package pl.gamilife.group.usecase.editgroupinvitationstatus;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.group.enums.InvitationStatusEnum;
import pl.gamilife.group.exception.domain.*;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupInvitation;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.model.InvitationStatus;
import pl.gamilife.group.repository.GroupInvitationJpaRepository;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.group.repository.InvitationStatusJpaRepository;
import pl.gamilife.group.service.GroupInvitationService;
import pl.gamilife.group.service.GroupMemberService;
import pl.gamilife.shared.kernel.exception.domain.GroupNotFoundException;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class EditGroupInvitationStatusUseCaseImpl implements EditGroupInvitationStatusUseCase {

    private final GroupInvitationJpaRepository groupInvitationRepository;
    private final InvitationStatusJpaRepository invitationStatusRepository;
    private final GroupMemberService groupMemberService;
    private final GroupJpaRepository groupRepository;
    private final GroupInvitationService groupInvitationService;

    @Override
    public EditGroupInvitationStatusResult execute(EditGroupInvitationStatusCommand cmd) {
        Group group = getGroupWithMembers(cmd.groupId());
        GroupInvitation groupInvitation = getGroupInvitationWithGroup(cmd.groupInvitationId(), cmd.groupId());
        InvitationStatus newInvitationStatus = getInvitationStatus(cmd.invitationStatusId());

        if (!groupInvitation.hasStatus(InvitationStatusEnum.SENT)) {
            throw new InvalidGroupInvitationDataException(
                    "Group invitation with status ACCEPTED, DECLINED or REVOKED are final and cannot be changed!");
        }

        if (newInvitationStatus.toEnum() == InvitationStatusEnum.SENT) {
            throw new InvalidGroupDataException(
                    "Group invitation with id: " + cmd.groupInvitationId() + " has already status: SENT");
        }

        boolean isUserReview = groupInvitation.doesBelongToUser(cmd.userId()) && (
                cmd.invitationStatusId() == InvitationStatusEnum.ACCEPTED.getId()
                        || cmd.invitationStatusId() == InvitationStatusEnum.DECLINED.getId()
        );
        boolean isAdminsCancellation = group.isUserAdmin(cmd.userId())
                && cmd.invitationStatusId() == InvitationStatusEnum.REVOKED.getId();
        if (!isUserReview && !isAdminsCancellation) {
            throw new ResourceOwnerPrivilegesRequiredException(
                    "Only user who is assigned to this invitation can review it and only the group admin can cancel it"
            );
        }

        if (isUserReview) {
            return processUserReview(groupInvitation, group, newInvitationStatus, cmd.token());
        }

        return processAdminCancellation(groupInvitation, newInvitationStatus);
    }

    private EditGroupInvitationStatusResult processUserReview(
            GroupInvitation groupInvitation,
            Group group,
            InvitationStatus newInvitationStatus,
            String token
    ) {
        if (!groupInvitationService.verifyToken(groupInvitation, token)) {
            throw new InvalidGroupInvitationTokenException("Invalid or tampered group invitation token!");
        }

        if (groupInvitation.isExpired()) {
            throw new GroupInvitationExpiredException("Group invitation has expired at: "
                    + groupInvitation.getExpiresAt());
        }

        // If invitation is accepted, create a new group member
        GroupMember groupMember = null;
        if (newInvitationStatus.toEnum() == InvitationStatusEnum.ACCEPTED) {
            groupMember = groupMemberService.createGroupMember(
                    group,
                    groupInvitation.getUserId()
            );
        }

        groupInvitation.changeStatus(newInvitationStatus);
        GroupInvitation savedGroupInvitation = groupInvitationRepository.save(groupInvitation);

        return buildEditGroupInvitationStatusResponse(
                savedGroupInvitation,
                groupMember != null ? groupMember.getId() : null
        );
    }

    private EditGroupInvitationStatusResult processAdminCancellation(
            GroupInvitation groupInvitation,
            InvitationStatus invitationStatus
    ) {
        groupInvitation.changeStatus(invitationStatus);
        GroupInvitation savedGroupInvitation = groupInvitationRepository.save(groupInvitation);

        return buildEditGroupInvitationStatusResponse(
                savedGroupInvitation,
                null
        );
    }

    private Group getGroupWithMembers(UUID groupId) {
        return groupRepository.findWithActiveMembersById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));
    }

    private InvitationStatus getInvitationStatus(Integer invitationStatusId) {
        return invitationStatusRepository.findById(invitationStatusId)
                .orElseThrow(() -> new InvitationStatusNotFoundException("Group invitation status with id: " +
                        invitationStatusId + " not found"));
    }

    private GroupInvitation getGroupInvitationWithGroup(UUID groupInvitationId, UUID groupId) {
        return groupInvitationRepository.findWithGroupByIdAndGroupId(groupInvitationId, groupId)
                .orElseThrow(() -> new GroupInvitationNotFoundException("Group invitation with id: " + groupInvitationId
                        + " not found!"));
    }

    private EditGroupInvitationStatusResult buildEditGroupInvitationStatusResponse(
            GroupInvitation groupInvitation,
            UUID groupMemberId
    ) {
        return EditGroupInvitationStatusResult.builder()
                .groupInvitationId(groupInvitation.getId())
                .groupInvited(new EditGroupInvitationStatusResult.GroupDto(
                        groupInvitation.getGroupId()
                ))
                .userId(groupInvitation.getUserId())
                .expiresAt(groupInvitation.getExpiresAt())
                .mailSentAt(groupInvitation.getCreatedAt())
                .invitationStatus(new EditGroupInvitationStatusResult.InvitationStatusDto(
                        groupInvitation.getStatus().getId(),
                        groupInvitation.getStatus().getTitle()
                ))
                .groupMemberId(groupMemberId)
                .build();
    }
}
