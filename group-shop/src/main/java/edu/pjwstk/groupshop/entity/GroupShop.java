package edu.pjwstk.groupshop.entity;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.*;

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
@Table(name = "group_shop")
public class GroupShop {
    @Id
    @Column(name = "group_shop_id", nullable = false)
    private UUID groupShopId;

    @Size(max = 100)
    @NotNull
    @Column(name = "name", nullable = false, length = 100)
    private String name;

    @Size(max = 500)
    @NotNull
    @Column(name = "description", nullable = false, length = 500)
    private String description;

    @NotNull
    @Column(name = "group_id", nullable = false)
    private UUID groupId;

    @NotNull
    @Column(name = "is_active", nullable = false)
    private Boolean isActive;

    @OneToMany(mappedBy = "groupShop")
    private Set<GroupItemInShop> groupItemInShops = new LinkedHashSet<>();


}
