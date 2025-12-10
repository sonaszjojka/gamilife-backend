package pl.gamilife.gamification.domain.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.validation.constraints.NotNull;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.hibernate.annotations.Immutable;
import pl.gamilife.shared.persistence.entity.BaseIntReadOnlyEntity;

import java.util.HashSet;
import java.util.Set;

@Getter
@Entity
@Immutable
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "level")
@ToString(exclude = {"items"})
public class Level extends BaseIntReadOnlyEntity {

    @NotNull
    @Column(name = "required_experience", nullable = false)
    private Integer requiredExperience;

    @OneToMany(mappedBy = "unlockLevel")
    private Set<Item> items = new HashSet<>();

    public Integer getLevel() {
        return getId();
    }

}