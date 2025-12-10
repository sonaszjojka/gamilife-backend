package pl.gamilife.gamification.domain.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
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
@Table(name = "item_slot")
@ToString(exclude = {"items"})
public class ItemSlot extends BaseIntReadOnlyEntity {

    @Size(max = 20)
    @NotNull
    @Column(name = "name", nullable = false, length = 20)
    private String name;

    @OneToMany(mappedBy = "itemSlot")
    private Set<Item> items = new HashSet<>();

}