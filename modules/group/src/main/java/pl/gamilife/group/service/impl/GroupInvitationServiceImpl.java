package pl.gamilife.group.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;
import pl.gamilife.group.enums.InvitationStatusEnum;
import pl.gamilife.group.exception.domain.InvitationStatusNotFoundException;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupInvitation;
import pl.gamilife.group.model.InvitationStatus;
import pl.gamilife.group.repository.GroupInvitationJpaRepository;
import pl.gamilife.group.repository.InvitationStatusJpaRepository;
import pl.gamilife.group.service.GroupInvitationService;
import pl.gamilife.shared.kernel.event.GroupInvitationCreatedEvent;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Base64;
import java.util.List;
import java.util.UUID;

@Slf4j
@Component
@RequiredArgsConstructor
public class GroupInvitationServiceImpl implements GroupInvitationService {

    private final GroupInvitationJpaRepository groupInvitationRepository;
    private final InvitationStatusJpaRepository invitationStatusRepository;
    private final ApplicationEventPublisher eventPublisher;

    @Value("${app.invitation.expiration-days}")
    private int groupInvitationExpirationDays;

    @Value("${app.invitation.recent-days}")
    private int groupInvitationRecentDays;

    @Value("${app.invitation.recent-days-limit}")
    private int groupInvitationRecentDaysLimit;

    private static String generateToken() {
        return UUID.randomUUID().toString();
    }

    private static String hashToken(String token) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hash = digest.digest(token.getBytes(StandardCharsets.UTF_8));
            return Base64.getUrlEncoder().encodeToString(hash);
        } catch (NoSuchAlgorithmException e) {
            log.error("Algorithm SHA-256 not found", e);
            throw new IllegalStateException("Algorithm SHA-256 not found");
        }
    }

    @Override
    public GroupInvitation createGroupInvitation(Group group, UUID userId) {
        if (!checkIfCanCreateAndRevokeAllPrevious(group, userId)) {
            throw new DomainValidationException("You have to wait a bit more to sent another invitation");
        }

        String token = generateToken();
        String tokenHash = hashToken(token);

        GroupInvitation groupInvitation = GroupInvitation.create(
                group,
                userId,
                groupInvitationExpirationDays,
                tokenHash,
                getInvitationStatusFromEnum(InvitationStatusEnum.SENT)
        );

        eventPublisher.publishEvent(new GroupInvitationCreatedEvent(
                userId,
                group.getId(),
                group.getName(),
                groupInvitation.getId(),
                token
        ));

        return groupInvitation;
    }

    private InvitationStatus getInvitationStatusFromEnum(InvitationStatusEnum invitationStatusEnum) {
        return invitationStatusRepository.findById(invitationStatusEnum.getId())
                .orElseThrow(() -> new InvitationStatusNotFoundException("Invitation status with id: "
                        + InvitationStatusEnum.SENT.getId() + " not found!"));
    }

    public boolean verifyToken(GroupInvitation groupInvitation, String token) {
        String hashedToken = hashToken(token);
        return hashedToken.equals(groupInvitation.getTokenHash());
    }

    private boolean checkIfCanCreateAndRevokeAllPrevious(Group group, UUID userId) {
        List<GroupInvitation> groupInvitations = groupInvitationRepository.findByGroupIdAndUserId(
                group.getId(),
                userId
        );

        if (groupInvitations.isEmpty()) {
            return true;
        }

        Instant recent = Instant.now().minus(groupInvitationRecentDays, ChronoUnit.DAYS);
        List<GroupInvitation> recentInvitations = groupInvitations.stream()
                .filter(gi -> gi.getCreatedAt().isAfter(recent))
                .toList();

        if (recentInvitations.size() > groupInvitationRecentDaysLimit) {
            return false;
        }

        revokeAllPrevious(groupInvitations);
        return true;
    }

    private void revokeAllPrevious(List<GroupInvitation> groupInvitations) {
        List<GroupInvitation> sent = groupInvitations.stream()
                .filter(gi -> gi.hasStatus(InvitationStatusEnum.SENT))
                .toList();
        sent.forEach(gi -> gi.changeStatus(getInvitationStatusFromEnum(InvitationStatusEnum.SENT)));
        groupInvitationRepository.saveAll(sent);
    }
}


