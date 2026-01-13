package pl.gamilife.group.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import pl.gamilife.group.enums.InvitationStatusEnum;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.assertj.core.api.Assertions.within;
import static org.instancio.Select.field;

class GroupInvitationTest {

    @Test
    void shouldCreateGroupInvitation_whenValidDataIsProvided() {
        // given
        Group group = Instancio.create(Group.class);
        UUID userId = UUID.randomUUID();
        int expiresInDays = 7;
        String token = "secure-token-hash";
        InvitationStatus status = Instancio.create(InvitationStatus.class);

        // when
        GroupInvitation invitation = GroupInvitation.create(group, userId, expiresInDays, token, status);

        // then
        assertThat(invitation).isNotNull();
        assertThat(invitation.getGroup()).isEqualTo(group);
        assertThat(invitation.getGroupId()).isEqualTo(group.getId());
        assertThat(invitation.getUserId()).isEqualTo(userId);
        assertThat(invitation.getTokenHash()).isEqualTo(token);
        assertThat(invitation.getStatus()).isEqualTo(status);
        assertThat(invitation.getStatusId()).isEqualTo(status.getId());
        assertThat(invitation.getExpiresAt()).isCloseTo(Instant.now().plus(expiresInDays, ChronoUnit.DAYS), within(1, ChronoUnit.SECONDS));
    }

    @Test
    void shouldReturnTrue_whenInvitationBelongsToUser() {
        // given
        UUID userId = UUID.randomUUID();
        GroupInvitation invitation = Instancio.of(GroupInvitation.class)
                .set(field(GroupInvitation::getUserId), userId)
                .create();

        // when
        boolean result = invitation.doesBelongToUser(userId);

        // then
        assertThat(result).isTrue();
    }

    @Test
    void shouldReturnFalse_whenInvitationDoesNotBelongToUser() {
        // given
        UUID userId = UUID.randomUUID();
        GroupInvitation invitation = Instancio.of(GroupInvitation.class)
                .set(field(GroupInvitation::getUserId), UUID.randomUUID())
                .create();

        // when
        boolean result = invitation.doesBelongToUser(userId);

        // then
        assertThat(result).isFalse();
    }

    @Test
    void shouldReturnTrue_whenInvitationIsExpired() {
        // given
        GroupInvitation invitation = Instancio.of(GroupInvitation.class)
                .set(field(GroupInvitation::getExpiresAt), Instant.now().minusSeconds(1))
                .create();

        // when
        boolean result = invitation.isExpired();

        // then
        assertThat(result).isTrue();
    }

    @Test
    void shouldReturnFalse_whenInvitationIsNotExpired() {
        // given
        GroupInvitation invitation = Instancio.of(GroupInvitation.class)
                .set(field(GroupInvitation::getExpiresAt), Instant.now().plusSeconds(1000))
                .create();

        // when
        boolean result = invitation.isExpired();

        // then
        assertThat(result).isFalse();
    }

    @Test
    void shouldReturnTrue_whenInvitationHasStatus() {
        // given
        InvitationStatusEnum expectedEnum = InvitationStatusEnum.values()[0];

        InvitationStatus status = Instancio.of(InvitationStatus.class)
                .set(field(InvitationStatus::getId), expectedEnum.getId())
                .create();

        GroupInvitation invitation = Instancio.of(GroupInvitation.class)
                .set(field(GroupInvitation::getStatus), status)
                .create();

        // when
        boolean result = invitation.hasStatus(expectedEnum);

        // then
        assertThat(result).isTrue();
    }

    @Test
    void shouldChangeStatus_whenStatusIsValid() {
        // given
        GroupInvitation invitation = Instancio.create(GroupInvitation.class);
        InvitationStatus newStatus = Instancio.create(InvitationStatus.class);

        // when
        invitation.changeStatus(newStatus);

        // then
        assertThat(invitation.getStatus()).isEqualTo(newStatus);
        assertThat(invitation.getStatusId()).isEqualTo(newStatus.getId());
    }

    @Test
    void shouldThrowException_whenChangingStatusToNull() {
        // given
        GroupInvitation invitation = Instancio.create(GroupInvitation.class);

        // when
        Throwable thrown = catchThrowable(() -> invitation.changeStatus(null));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Invitation status cannot be null");
    }

    @Test
    void shouldThrowException_whenGroupIsNull() {
        // given
        UUID userId = UUID.randomUUID();
        String token = "token";
        InvitationStatus status = Instancio.create(InvitationStatus.class);

        // when
        Throwable thrown = catchThrowable(() -> GroupInvitation.create(null, userId, 1, token, status));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Group cannot be null");
    }

    @Test
    void shouldThrowException_whenUserIdIsNull() {
        // given
        Group group = Instancio.create(Group.class);
        String token = "token";
        InvitationStatus status = Instancio.create(InvitationStatus.class);

        // when
        Throwable thrown = catchThrowable(() -> GroupInvitation.create(group, null, 1, token, status));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("User id cannot be null");
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "   "})
    void shouldThrowException_whenTokenIsInvalid(String invalidToken) {
        // given
        Group group = Instancio.create(Group.class);
        UUID userId = UUID.randomUUID();
        InvitationStatus status = Instancio.create(InvitationStatus.class);

        // when
        Throwable thrown = catchThrowable(() -> GroupInvitation.create(group, userId, 1, invalidToken, status));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Token cannot be null or empty");
    }

    @Test
    void shouldThrowException_whenTokenIsNull() {
        // given
        Group group = Instancio.create(Group.class);
        UUID userId = UUID.randomUUID();
        InvitationStatus status = Instancio.create(InvitationStatus.class);

        // when
        Throwable thrown = catchThrowable(() -> GroupInvitation.create(group, userId, 1, null, status));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Token cannot be null or empty");
    }

    @Test
    void shouldThrowException_whenInitialStatusIsNull() {
        // given
        Group group = Instancio.create(Group.class);
        UUID userId = UUID.randomUUID();
        String token = "token";

        // when
        Throwable thrown = catchThrowable(() -> GroupInvitation.create(group, userId, 1, token, null));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Invitation status cannot be null");
    }
}