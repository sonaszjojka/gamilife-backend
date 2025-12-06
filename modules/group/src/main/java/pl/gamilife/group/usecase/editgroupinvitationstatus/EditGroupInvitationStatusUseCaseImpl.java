package pl.gamilife.group.usecase.editgroupinvitationstatus;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.group.enums.InvitationStatusEnum;
import pl.gamilife.group.exception.domain.*;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupInvitation;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.model.InvitationStatus;
import pl.gamilife.group.repository.GroupInvitationJpaRepository;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.group.repository.InvitationStatusJpaRepository;
import pl.gamilife.group.service.GroupMemberService;
import pl.gamilife.group.util.GroupInvitationUtil;
import pl.gamilife.infrastructure.core.exception.domain.GroupNotFoundException;
import pl.gamilife.infrastructure.core.exception.domain.ResourceOwnerPrivilegesRequiredException;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class EditGroupInvitationStatusUseCaseImpl implements EditGroupInvitationStatusUseCase {

    private final GroupInvitationJpaRepository groupInvitationRepository;
    private final InvitationStatusJpaRepository invitationStatusRepository;
    private final AuthApi authApi;
    private final GroupMemberService groupMemberService;
    private final GroupInvitationUtil groupInvitationUtil;
    private final GroupJpaRepository groupRepository;

    @Override
    public EditGroupInvitationStatusResult execute(EditGroupInvitationStatusCommand cmd) {
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
