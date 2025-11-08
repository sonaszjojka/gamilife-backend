package edu.pjwstk.groups.usecase.editgroupinvitationstatus;

import edu.pjwstk.groups.model.GroupInvitation;
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberResponse;
import org.springframework.stereotype.Component;

@Component
public class EditGroupInvitationStatusMapperImpl implements EditGroupInvitationStatusMapper {

    @Override
    public EditGroupInvitationStatusResponse toResponse(GroupInvitation savedGroupInvitation,
                                                        CreateGroupMemberResponse groupMember) {
        if (savedGroupInvitation == null) {
            return null;
        }

        return EditGroupInvitationStatusResponse.builder()
                .groupInvitationId(savedGroupInvitation.getGroupInvitationId())
                .groupInvited(
                        savedGroupInvitation.getGroupInvited() != null
                                ? EditGroupInvitationStatusResponse.GroupDto.builder()
                                .groupId(savedGroupInvitation.getGroupInvited().getGroupId())
                                .build()
                                : null
                )
                .userId(savedGroupInvitation.getUserId())
                .expiresAt(savedGroupInvitation.getExpiresAt())
                .mailSentAt(savedGroupInvitation.getMailSentAt())
                .link(savedGroupInvitation.getLink())
                .invitationStatus(
                        savedGroupInvitation.getInvitationStatus() != null
                                ? EditGroupInvitationStatusResponse.InvitationStatusDto.builder()
                                .invitationStatusId(savedGroupInvitation.getInvitationStatus().getInvitationStatusId())
                                .title(savedGroupInvitation.getInvitationStatus().getTitle())
                                .build() : null
                )
                .groupMemberResponse(groupMember)
                .build();
    }

}
