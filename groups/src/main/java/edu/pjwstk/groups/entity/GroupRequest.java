package edu.pjwstk.groups.entity;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;
import java.util.UUID;

@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "group_request")
public class GroupRequest {

    @Id
    @Column(name = "group_request_id", nullable = false, updatable = false, unique = true)
    private UUID groupRequestId;

    @Column(name = "user_id", nullable = false, updatable = false)
    private UUID userId;

    @ManyToOne
    @JoinColumn(name = "group_id", nullable = false, updatable = false)
    private Group groupRequested;

    @Column(name = "created_at")
    protected Instant createdAt;

    @PrePersist
    public void prePersist() {
        this.createdAt = Instant.now();
    }

    @ManyToOne
    @JoinColumn(name = "status_id", nullable = false)
    private GroupRequestStatus groupRequestStatus;

}
