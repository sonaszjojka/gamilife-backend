package pl.gamilife.group.service.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import pl.gamilife.group.enums.InvitationStatusEnum;
import pl.gamilife.group.exception.domain.InvitationStatusNotFoundException;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupInvitation;
import pl.gamilife.group.model.InvitationStatus;
import pl.gamilife.group.repository.InvitationStatusJpaRepository;
import pl.gamilife.group.service.GroupInvitationService;

import java.util.UUID;

@Component
@RequiredArgsConstructor
public class GroupInvitationServiceImpl implements GroupInvitationService {

    private final InvitationStatusJpaRepository invitationStatusRepository;
    @Value("${app.invitation.expiration-days}")
    private int groupInvitationExpirationDays;
    @Value("${app.invitation.invitation-url-prefix}")
    private String invitationUrlPrefix;

    @Override
    public GroupInvitation createGroupInvitation(Group group, UUID userId) {
        return GroupInvitation.create(
                group,
                userId,
                groupInvitationExpirationDays,
                invitationUrlPrefix,
                getSentInvitationStatus()
        );
    }

    private InvitationStatus getSentInvitationStatus() {
        return invitationStatusRepository.findById(InvitationStatusEnum.SENT.getId())
                .orElseThrow(() -> new InvitationStatusNotFoundException("Invitation status with id: "
                        + InvitationStatusEnum.SENT.getId() + " not found!"));
    }
}


