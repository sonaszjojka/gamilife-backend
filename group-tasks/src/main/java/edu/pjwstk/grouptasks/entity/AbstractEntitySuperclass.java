package edu.pjwstk.grouptasks.entity;

import jakarta.persistence.Column;
import jakarta.persistence.MappedSuperclass;
import jakarta.persistence.PrePersist;
import jakarta.persistence.PreUpdate;
import lombok.*;

import java.io.Serializable;
import java.time.Instant;

@MappedSuperclass
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor(access = AccessLevel.PROTECTED)
@Getter
@Setter
public abstract class AbstractEntitySuperclass implements Serializable {



    @Column(name = "last_edit")
    protected Instant lastEdit;

    @PrePersist
    public void prePersist() {
        this.lastEdit = Instant.now();
    }

    @PreUpdate
    public void preUpdate() {
        this.lastEdit = Instant.now();
    }
}
