package edu.pjwstk.groups.usecase.creategroupinvitation;

import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupInvitation;
import edu.pjwstk.groups.model.InvitationStatus;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.UUID;

@Component
public class CreateGroupInvitationMapperImpl implements CreateGroupInvitationMapper {

    @Override
    public GroupInvitation toEntity(Group groupInvited, InvitationStatus invitationStatus,
                                    UUID userId, LocalDateTime expiresAt, String link,
                                    UUID groupInvitationId, String tokenHash) {
        return GroupInvitation.builder()
                .groupInvitationId(groupInvitationId)
                .userId(userId)
                .groupInvited(groupInvited)
                .expiresAt(expiresAt)
                .mailSentAt(LocalDateTime.now())
                .link(link)
                .tokenHash(tokenHash)
                .invitationStatus(invitationStatus)
                .build();
    }

    @Override
    public CreateGroupInvitationResponse toResponse(GroupInvitation savedGroupInvitation) {
        if (savedGroupInvitation == null) {
            return null;
        }

        return CreateGroupInvitationResponse.builder()
                .groupInvitationId(savedGroupInvitation.getGroupInvitationId())
                .groupInvited(
                        savedGroupInvitation.getGroupInvited() != null
                                ? CreateGroupInvitationResponse.GroupDto.builder()
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
                                ? CreateGroupInvitationResponse.InvitationStatusDto.builder()
                                .invitationStatusId(savedGroupInvitation.getInvitationStatus().getInvitationStatusId())
                                .title(savedGroupInvitation.getInvitationStatus().getTitle())
                                .build() : null
                )
                .build();
    }

}
