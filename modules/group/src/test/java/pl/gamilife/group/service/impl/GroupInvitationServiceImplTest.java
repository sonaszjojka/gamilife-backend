package pl.gamilife.group.service.impl;

import org.instancio.Instancio;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.util.ReflectionTestUtils;
import pl.gamilife.group.enums.InvitationStatusEnum;
import pl.gamilife.group.exception.domain.InvitationStatusNotFoundException;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupInvitation;
import pl.gamilife.group.model.InvitationStatus;
import pl.gamilife.group.repository.GroupInvitationJpaRepository;
import pl.gamilife.group.repository.InvitationStatusJpaRepository;
import pl.gamilife.shared.kernel.event.GroupInvitationCreatedEvent;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Base64;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.instancio.Select.field;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class GroupInvitationServiceImplTest {

    @Mock
    private GroupInvitationJpaRepository groupInvitationRepository;

    @Mock
    private InvitationStatusJpaRepository invitationStatusRepository;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @InjectMocks
    private GroupInvitationServiceImpl groupInvitationService;

    @BeforeEach
    void setUp() {
        ReflectionTestUtils.setField(groupInvitationService, "groupInvitationExpirationDays", 7);
        ReflectionTestUtils.setField(groupInvitationService, "groupInvitationRecentDays", 1);
        ReflectionTestUtils.setField(groupInvitationService, "groupInvitationRecentDaysLimit", 5);
    }

    @Test
    void shouldCreateGroupInvitation_whenNoPreviousInvitationsExist() {
        // given
        Group group = Instancio.create(Group.class);
        UUID userId = UUID.randomUUID();
        InvitationStatus status = Instancio.of(InvitationStatus.class)
                .set(field(InvitationStatus::getId), InvitationStatusEnum.SENT.getId())
                .create();

        when(groupInvitationRepository.findByGroupIdAndUserId(group.getId(), userId))
                .thenReturn(Collections.emptyList());
        when(invitationStatusRepository.findById(InvitationStatusEnum.SENT.getId()))
                .thenReturn(Optional.of(status));

        // when
        GroupInvitation result = groupInvitationService.createGroupInvitation(group, userId);

        // then
        assertThat(result).isNotNull();
        assertThat(result.getStatus()).isEqualTo(status);

        verify(groupInvitationRepository, never()).saveAll(anyList());
        verify(eventPublisher).publishEvent(any(GroupInvitationCreatedEvent.class));
    }

    @Test
    void shouldCreateGroupInvitation_andRevokePrevious_whenLimitNotExceeded() {
        // given
        Group group = Instancio.create(Group.class);
        UUID userId = UUID.randomUUID();

        InvitationStatus sentStatus = Instancio.of(InvitationStatus.class)
                .set(field(InvitationStatus::getId), InvitationStatusEnum.SENT.getId())
                .create();

        GroupInvitation oldInvitation = Instancio.of(GroupInvitation.class)
                .set(field(GroupInvitation::getStatus), sentStatus)
                .set(field(GroupInvitation::getCreatedAt), Instant.now().minus(100, ChronoUnit.DAYS))
                .create();

        when(groupInvitationRepository.findByGroupIdAndUserId(group.getId(), userId))
                .thenReturn(List.of(oldInvitation));
        when(invitationStatusRepository.findById(InvitationStatusEnum.SENT.getId()))
                .thenReturn(Optional.of(sentStatus));

        // when
        groupInvitationService.createGroupInvitation(group, userId);

        // then
        verify(groupInvitationRepository).saveAll(anyList());
        verify(eventPublisher).publishEvent(any(GroupInvitationCreatedEvent.class));
    }

    @Test
    void shouldThrowException_whenRecentInvitationsLimitExceeded() {
        // given
        Group group = Instancio.create(Group.class);
        UUID userId = UUID.randomUUID();

        List<GroupInvitation> recentInvitations = Instancio.ofList(GroupInvitation.class)
                .size(6)
                .set(field(GroupInvitation::getCreatedAt), Instant.now())
                .create();

        when(groupInvitationRepository.findByGroupIdAndUserId(group.getId(), userId))
                .thenReturn(recentInvitations);

        // when
        Throwable thrown = catchThrowable(() -> groupInvitationService.createGroupInvitation(group, userId));

        // then
        assertThat(thrown)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("You have to wait a bit more to sent another invitation");

        verify(eventPublisher, never()).publishEvent(any());
    }

    @Test
    void shouldThrowException_whenInvitationStatusNotFound() {
        // given
        Group group = Instancio.create(Group.class);
        UUID userId = UUID.randomUUID();

        when(groupInvitationRepository.findByGroupIdAndUserId(group.getId(), userId))
                .thenReturn(Collections.emptyList());
        when(invitationStatusRepository.findById(anyInt()))
                .thenReturn(Optional.empty());

        // when
        Throwable thrown = catchThrowable(() -> groupInvitationService.createGroupInvitation(group, userId));

        // then
        assertThat(thrown)
                .isInstanceOf(InvitationStatusNotFoundException.class)
                .hasMessageContaining("not found");
    }

    @Test
    void shouldVerifyToken_whenTokenIsValid() throws Exception {
        // given
        String token = "valid-token";

        MessageDigest digest = MessageDigest.getInstance("SHA-256");
        byte[] hash = digest.digest(token.getBytes(StandardCharsets.UTF_8));
        String expectedHash = Base64.getUrlEncoder().encodeToString(hash);

        GroupInvitation invitation = Instancio.of(GroupInvitation.class)
                .set(field(GroupInvitation::getTokenHash), expectedHash)
                .create();

        // when
        boolean result = groupInvitationService.verifyToken(invitation, token);

        // then
        assertThat(result).isTrue();
    }

    @Test
    void shouldVerifyToken_whenTokenIsInvalid() {
        // given
        GroupInvitation invitation = Instancio.create(GroupInvitation.class);
        String invalidToken = "invalid-token";

        // when
        boolean result = groupInvitationService.verifyToken(invitation, invalidToken);

        // then
        assertThat(result).isFalse();
    }

    @Test
    void shouldPublishCorrectEventData() {
        // given
        Group group = Instancio.create(Group.class);
        UUID userId = UUID.randomUUID();
        InvitationStatus status = Instancio.of(InvitationStatus.class)
                .set(field(InvitationStatus::getId), InvitationStatusEnum.SENT.getId())
                .create();

        when(groupInvitationRepository.findByGroupIdAndUserId(any(), any())).thenReturn(Collections.emptyList());
        when(invitationStatusRepository.findById(anyInt())).thenReturn(Optional.of(status));

        // when
        groupInvitationService.createGroupInvitation(group, userId);

        // then
        ArgumentCaptor<GroupInvitationCreatedEvent> eventCaptor = ArgumentCaptor.forClass(GroupInvitationCreatedEvent.class);
        verify(eventPublisher).publishEvent(eventCaptor.capture());

        GroupInvitationCreatedEvent event = eventCaptor.getValue();
        assertThat(event.userId()).isEqualTo(userId);
        assertThat(event.groupId()).isEqualTo(group.getId());
        assertThat(event.token()).isNotBlank();
    }

    @Test
    void shouldThrowException_whenSha256AlgorithmIsNotFound() {
        // given
        GroupInvitation invitation = Instancio.create(GroupInvitation.class);
        String token = "anyToken";

        try (MockedStatic<MessageDigest> mockedDigest = mockStatic(MessageDigest.class)) {
            mockedDigest.when(() -> MessageDigest.getInstance("SHA-256"))
                    .thenThrow(new NoSuchAlgorithmException());

            // when
            Throwable thrown = catchThrowable(() -> groupInvitationService.verifyToken(invitation, token));

            // then
            assertThat(thrown)
                    .isInstanceOf(IllegalStateException.class)
                    .hasMessage("Algorithm SHA-256 not found");
        }
    }
}