package pl.gamilife.groupshop.entity;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
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
@Table(name = "owned_group_item")
public class OwnedGroupItem {
    @Id
    @Column(name = "owned_group_item_id", nullable = false)
    private UUID ownedGroupItemId;

    @NotNull
    @Column(name = "group_member_id", nullable = false)
    private UUID groupMemberId;

    @NotNull
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "group_item_in_shop_id", nullable = false)
    @ToString.Exclude
    private GroupItemInShop groupItemInShop;

    @NotNull
    @Builder.Default
    @Column(name = "is_used_up", nullable = false)
    private Boolean isUsedUp = false;

    @Column(name = "use_date")
    private Instant useDate;

}
