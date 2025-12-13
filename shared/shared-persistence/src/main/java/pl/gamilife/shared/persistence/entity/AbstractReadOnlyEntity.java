package pl.gamilife.shared.persistence.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.Immutable;

import java.io.Serializable;
import java.util.Objects;

@Getter
@Immutable
@MappedSuperclass
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public abstract class AbstractReadOnlyEntity<K extends Serializable> {

    @Id
    private K id;

    @PrePersist
    public void prePersist() {
        throw new UnsupportedOperationException(
                String.format("INSERT is forbidden for read-only entity: %s", getClass().getName())
        );
    }

    @PreRemove
    public void preRemove() {
        throw new UnsupportedOperationException(
                String.format("DELETE is forbidden for read-only entity: %s", getClass().getName())
        );
    }

    @PreUpdate
    public void preUpdate() {
        throw new UnsupportedOperationException(
                String.format("UPDATE is forbidden for read-only entity: %s", getClass().getName())
        );
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        AbstractReadOnlyEntity<?> that = (AbstractReadOnlyEntity<?>) o;
        return Objects.equals(id, that.getId());
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
