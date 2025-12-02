package pl.gamilife.communication.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

import java.time.Instant;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

@Getter
@Setter
@Entity
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "notification_retry")
public class NotificationRetry {
    @Id
    @Builder.Default
    @Column(name = "id", nullable = false)
    private UUID id = UUID.randomUUID();

    @NotNull
    @Column(name = "user_id", nullable = false)
    private UUID userId;

    @Size(max = 100)
    @NotNull
    @Column(name = "title", nullable = false, length = 100)
    private String title;

    @Size(max = 255)
    @NotNull
    @Column(name = "message", nullable = false)
    private String message;

    @NotNull
    @Column(name = "original_timestamp", nullable = false)
    private Instant originalTimestamp;

    @Column(name = "data", nullable = false)
    @JdbcTypeCode(SqlTypes.JSON)
    private Map<String, Object> data;

    @NotNull
    @Column(name = "notification_type_id", nullable = false)
    private Integer notificationTypeId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "notification_type_id", nullable = false, updatable = false, insertable = false)
    private NotificationType notificationType;

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        NotificationRetry that = (NotificationRetry) o;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}