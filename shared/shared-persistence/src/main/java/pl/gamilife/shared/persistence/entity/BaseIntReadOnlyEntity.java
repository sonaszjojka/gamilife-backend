package pl.gamilife.shared.persistence.entity;

import jakarta.persistence.MappedSuperclass;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.Immutable;

@Getter
@Immutable
@MappedSuperclass
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public abstract class BaseIntReadOnlyEntity extends AbstractReadOnlyEntity<Integer> {
}
