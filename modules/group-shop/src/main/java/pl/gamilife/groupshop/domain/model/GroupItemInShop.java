package pl.gamilife.groupshop.domain.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.time.Instant;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.UUID;

@Getter
@Setter
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "group_item_in_shop")
public class GroupItemInShop {


    @Id
    @Column(name = "group_item_in_shop_id", nullable = false)
    private UUID groupItemId;

    @NotNull
    @Column(name = "price", nullable = false)
    private Integer price;

    @NotNull
    @Column(name = "name", nullable = false, length = 30)
    private String name;

    @NotNull
    @Column(name = "created_at", nullable = false)
    private Instant createdAt;

    @NotNull
    @Builder.Default
    @Column(name = "is_active", nullable = false)
    private Boolean isActive = false;

    @NotNull
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "group_shop_id", nullable = false)
    private GroupShop groupShop;

    @Builder.Default
    @OneToMany(mappedBy = "groupItemInShop")
    private Set<OwnedGroupItem> ownedGroupItems = new LinkedHashSet<>();

}
