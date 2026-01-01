package pl.gamilife.communication.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;
import pl.gamilife.shared.persistence.entity.BaseEntity;

import java.time.Instant;
import java.util.Map;
import java.util.UUID;

@Getter
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "notification_retry", schema = "communication")
public class NotificationRetry extends BaseEntity {

    @NotNull
    @Column(name = "user_id", nullable = false)
    private UUID userId;

    @NotNull
    @Column(name = "original_timestamp", nullable = false)
    private Instant originalTimestamp;

    @Setter
    @Column(name = "data")
    @JdbcTypeCode(SqlTypes.JSON)
    private Map<String, Object> data;

    @NotNull
    @Column(name = "notification_type_id", nullable = false)
    private Integer notificationTypeId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "notification_type_id", nullable = false, updatable = false, insertable = false)
    private NotificationType notificationType;

    private NotificationRetry(UUID userId, Instant originalTimestamp, Map<String, Object> data, Integer notificationTypeId) {
        setUserId(userId);
        setOriginalTimestamp(originalTimestamp);
        setData(data);
        setNotificationTypeId(notificationTypeId);
    }

    public static NotificationRetry create(UUID userId, Instant originalTimestamp, Map<String, Object> data, Integer notificationTypeId) {
        return new NotificationRetry(userId, originalTimestamp, data, notificationTypeId);
    }

    private void setUserId(UUID userId) {
        if (userId == null) {
            throw new DomainValidationException("User ID cannot be null");
        }

        this.userId = userId;
    }

    private void setOriginalTimestamp(Instant originalTimestamp) {
        if (originalTimestamp == null) {
            throw new DomainValidationException("Original timestamp cannot be null");
        }

        if (originalTimestamp.isAfter(Instant.now())) {
            throw new DomainValidationException("Original timestamp cannot be in the future");
        }

        this.originalTimestamp = originalTimestamp;
    }

    private void setNotificationTypeId(Integer notificationTypeId) {
        if (notificationTypeId == null) {
            throw new DomainValidationException("Notification type cannot be null");
        }

        this.notificationTypeId = notificationTypeId;
    }

}