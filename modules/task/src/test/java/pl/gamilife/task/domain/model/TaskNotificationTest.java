package pl.gamilife.task.domain.model;

import org.junit.jupiter.api.Test;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;

import java.time.Instant;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;

class TaskNotificationTest {

    @Test
    void shouldCreateTaskNotification_whenValidDataIsProvided() {
        // given
        UUID taskId = UUID.randomUUID();
        Instant sendAt = Instant.now().plusSeconds(3600);

        // when
        TaskNotification notification = TaskNotification.create(taskId, sendAt);

        // then
        assertThat(notification).isNotNull();
        assertThat(notification.getTaskId()).isEqualTo(taskId);
        assertThat(notification.getSendAt()).isEqualTo(sendAt);
    }

    @Test
    void shouldUpdateSendAt_whenNewSendAtIsInFuture() {
        // given
        UUID taskId = UUID.randomUUID();
        Instant sendAt = Instant.now().plusSeconds(3600);
        TaskNotification notification = TaskNotification.create(taskId, sendAt);
        Instant newSendAt = Instant.now().plusSeconds(7200);

        // when
        notification.setSendAt(newSendAt);

        // then
        assertThat(notification.getSendAt()).isEqualTo(newSendAt);
    }

    @Test
    void shouldThrowException_whenNewSendAtIsInPast() {
        // given
        UUID taskId = UUID.randomUUID();
        Instant sendAt = Instant.now().plusSeconds(3600);
        TaskNotification notification = TaskNotification.create(taskId, sendAt);
        Instant newSendAt = Instant.now().minusSeconds(3600);

        // when
        Throwable throwable = catchThrowable(() -> notification.setSendAt(newSendAt));

        // then
        assertThat(throwable)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Send date cannot be in the past");
    }
}
