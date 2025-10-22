package edu.pjwstk.groupshop.entity;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.*;
import java.time.Instant;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.UUID;

@Getter
@Setter
@ToString
@Builder
@Entity
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "group_item_in_shop")
public class GroupItemInShop {


    @Id
    @Column(name = "group_item_in_shop_id", nullable = false)
    private UUID id;

    @NotNull
    @Column(name = "price", nullable = false)
    private Integer price;

    @NotNull
    @Column(name = "created_at", nullable = false)
    private Instant createdAt;

    @NotNull
    @Column(name = "is_active", nullable = false)
    private Boolean isActive = false;

    @NotNull
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "group_shop_id", nullable = false)
    private GroupShop groupShop;

    @OneToMany(mappedBy = "groupItemInShop")
    private Set<OwnedGroupItem> ownedGroupItems = new LinkedHashSet<>();

}
