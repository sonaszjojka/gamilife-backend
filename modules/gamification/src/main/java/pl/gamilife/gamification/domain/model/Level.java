package pl.gamilife.gamification.domain.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.Getter;
import lombok.ToString;
import org.hibernate.annotations.Immutable;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

@Getter
@Entity
@Table(name = "level")
@ToString(exclude = {"items"})
@Immutable
public class Level {
    @Id
    @Column(name = "level", nullable = false)
    private Integer level;

    @NotNull
    @Column(name = "required_experience", nullable = false)
    private Integer requiredExperience;

    @OneToMany(mappedBy = "unlockLevel")
    private Set<Item> items = new HashSet<>();

    @Override
    public boolean equals(Object o) {
        if (o == null || getClass() != o.getClass()) return false;
        Level that = (Level) o;
        return Objects.equals(level, that.level);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(level);
    }
}